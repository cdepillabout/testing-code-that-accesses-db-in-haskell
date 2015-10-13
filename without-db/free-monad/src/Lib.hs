{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-- This is an unfortunate hack.  Used to make the code slightly easier to
-- follow.  See below for how we could fix it.
{-# LANGUAGE UndecidableInstances       #-}

-- This is another unfortunate hack to make the code simpler and easier to
-- understand.  Described at the end of this file.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib where

import Control.Exception (Exception)
import Control.Monad.Catch (catch, throwM)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import qualified Control.Monad.Operational as O
import Control.Monad.Operational hiding (view)
import Control.Monad.Trans.Either (EitherT)
import Data.Proxy (Proxy(..))
import Database.Persist
    ( Entity, Key, PersistEntity, PersistEntityBackend, ToBackendKey, Unique
    , delete, get, getBy, insert, replace )
import Database.Persist.Sqlite
    ( SqlBackend, SqlPersistT, runMigration, runSqlConn, toSqlKey
    , withSqliteConn )
import Database.Persist.TH
    ( mkMigrate, mkPersist, persistLowerCase, share, sqlSettings )
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant
    ( (:<|>)(..), (:>), Capture, Delete, FromText(..), Get, JSON, Post, Put
    , ReqBody, ServantErr(..), Server, err404, serve )

-- Inspired by
-- https://hbtvl.wordpress.com/2015/06/28/servant-persistent-and-dsls.

instance Exception ServantErr

----------------------------------
-- Persistent model definitions --
----------------------------------

share [ mkPersist sqlSettings, mkMigrate "migrateAll"]
      [persistLowerCase|
Author json
    name Text
    UniqueName name
    deriving Show

BlogPost json
    title Text
    content Text
    author AuthorId
    deriving Show
|]


-- XXX: Hack.
instance (ToBackendKey SqlBackend a) => FromText (Key a) where
    fromText :: Text -> Maybe (Key a)
    fromText text = toSqlKey <$> fromText text

-------------------------
-- DSL for persistent? --
-------------------------

type DbDSL = Program DbAction
type PC val = (PersistEntityBackend val ~ SqlBackend, PersistEntity val)
data DbAction a where
    ThrowDb  :: ServantErr               -> DbAction a
    GetDb    :: PC val => Key val        -> DbAction (Maybe val)
    GetByDb  :: PC val => Unique val     -> DbAction (Maybe (Entity val))
    InsertDb :: PC val =>            val -> DbAction (Key val)
    UpdateDb :: PC val => Key val -> val -> DbAction ()
    DelDb    :: PC val => Key val        -> DbAction ()

-- | throws an error
throwDb :: ServantErr -> DbDSL a
throwDb err = singleton (ThrowDb err)

-- | dual of `persistent`'s `get`
getDb :: PC val => Key val -> DbDSL (Maybe val)
getDb key = singleton (GetDb key)

-- | dual of `persistent`'s `getBy`
getByDb :: PC val => Unique val ->  DbDSL (Maybe (Entity val))
getByDb uniqueVal = singleton (GetByDb uniqueVal)

-- | dual of `persistent`'s `insert`
insertDb :: PC val => val ->  DbDSL (Key val)
insertDb val = singleton (InsertDb val)

-- | dual of `persistent`'s `update`
updateDb :: PC val => Key val -> val -> DbDSL ()
updateDb key val = singleton (UpdateDb key val)

-- | dual of `persistent`'s `delete`
deleteDb :: PC val => Key val -> DbDSL ()
deleteDb key = singleton (DelDb key)

-- | like `getDb` but throws a 404 if it could not find the corresponding record
getOr404Db :: PC val => Key val -> DbDSL val
getOr404Db key = do
    maybeVal <- getDb key
    case maybeVal of
        Just val -> return val
        Nothing -> throwDb err404

-- | like `getByDb` but throws a 404 if it could not find the corresponding record
getByOr404Db :: PC val => Unique val -> DbDSL (Entity val)
getByOr404Db uniqueVal = do
    maybeEntity <- getByDb uniqueVal
    case maybeEntity of
        Just entity -> return entity
        Nothing -> throwDb err404

runDbDSLInPersistent :: DbDSL a -> SqlPersistT (EitherT ServantErr IO) a
runDbDSLInPersistent ws =
    case O.view ws of
        Return a -> return a
        a :>>= nextStep -> runM a nextStep
  where
    runM :: DbAction a -> (a -> DbDSL b) -> SqlPersistT (EitherT ServantErr IO) b
    runM (GetDb key) nextStep = do
        maybeVal <- get key
        runDbDSLInPersistent $ nextStep maybeVal
    runM (InsertDb val) nextStep = do
        key <- insert val
        runDbDSLInPersistent $ nextStep key
    runM (DelDb key) nextStep = do
        delete key
        runDbDSLInPersistent $ nextStep ()
    runM (GetByDb uniqueVal) nextStep = do
        maybeEntityVal <- getBy uniqueVal
        runDbDSLInPersistent $ nextStep maybeEntityVal
    runM (UpdateDb key val) nextStep = do
        replace key val
        runDbDSLInPersistent $ nextStep ()
    runM (ThrowDb servantErr@(ServantErr httpStatusCode httpStatusString _ _)) _ = do
        -- In actual usage, you may need to rollback the database
        -- connection here.  It doesn't matter for this simple
        -- demonstration, but in production you'll probably want to roll
        -- back the current transaction when you use 'Throw'.
        -- conn <- ask
        -- liftIO $ connRollback conn (getStmtConn conn)
        liftIO $ putStrLn $ "error occured: " ++ show (httpStatusCode,httpStatusString)
        throwM servantErr

-----------------
-- servant api --
-----------------

type CRUD a =                         ReqBody '[JSON] a :> Post '[JSON] (Key a) -- create
         :<|> Capture "id" (Key a)                      :> Get '[JSON] a -- read
         :<|> Capture "id" (Key a) :> ReqBody '[JSON] a :> Put '[JSON] () -- update
         :<|> Capture "id" (Key a)                      :> Delete '[JSON] () -- delete

-------------------------------------------

runCreateRest :: (PersistEntity a, ToBackendKey SqlBackend a)
       => SqlBackend
       -> a
       -> EitherT ServantErr IO (Key a)
runCreateRest conn val = runDbDSLInServant conn $ insertDb val

runReadRest :: (PersistEntity a, ToBackendKey SqlBackend a)
       => SqlBackend
       -> Key a
       -> EitherT ServantErr IO a
runReadRest conn key = runDbDSLInServant conn $ getOr404Db key

runUpdateRest :: (PersistEntity a, ToBackendKey SqlBackend a)
       => SqlBackend
       -> Key a
       -> a
       -> EitherT ServantErr IO ()
runUpdateRest conn key val = runDbDSLInServant conn $ updateDb key val

runDeleteRest :: (PersistEntity a, ToBackendKey SqlBackend a)
       => SqlBackend
       -> Key a
       -> EitherT ServantErr IO ()
runDeleteRest conn key = runDbDSLInServant conn $ deleteDb key

runDbDSLInServant :: forall a . SqlBackend -> DbDSL a -> EitherT ServantErr IO a
runDbDSLInServant conn dbDSL = runSqlConn (runDbDSLInPersistent dbDSL) conn
                                `catch` \(err::ServantErr) -> throwError err

server :: SqlBackend
       -> Server ( "author" :> CRUD Author
              :<|> "post"   :> CRUD BlogPost )
server conn =
         -- Servant HTTP handlers for Author CRUD requests.
            ( runCreateRest conn  -- Create Author
         :<|> runReadRest conn  -- Read Author
         :<|> runUpdateRest conn  -- Update Author
         :<|> runDeleteRest conn  -- Delete Author
            )
            :<|>
         -- Servant HTTP handlers for BlogPost CRUD requests.
            ( runCreateRest conn  -- Create BlogPost
         :<|> runReadRest conn  -- Read BlogPost
         :<|> runUpdateRest conn  -- Update BlogPost
         :<|> runDeleteRest conn  -- Delete BlogPost
            )

defaultMain :: IO ()
defaultMain =
    runStderrLoggingT $ withSqliteConn ":memory:" $ \conn -> do
        liftIO $ runSqlConn (runMigration migrateAll) conn
        liftIO $ putStrLn "\napi running on port 8080..."
        liftIO $ run 8080 $ serve myApiType $ server conn
  where
    myApiType :: Proxy ( "author" :> CRUD Author :<|> "post" :> CRUD BlogPost )
    myApiType = Proxy
