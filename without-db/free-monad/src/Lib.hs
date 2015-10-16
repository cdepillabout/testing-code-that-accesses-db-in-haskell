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
{-# LANGUAGE RankNTypes                 #-}
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
import Control.Monad.Operational (Program, ProgramViewT(..), singleton, view)
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

----------------------------------
-- Persistent model definitions --
----------------------------------

share [ mkPersist sqlSettings, mkMigrate "migrateAll"]
      [persistLowerCase|
BlogPost json
    title Text
    content Text
    deriving Show
|]


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

runDbDSLInPersistent :: DbDSL a -> SqlPersistT (EitherT ServantErr IO) a
runDbDSLInPersistent dbDSL =
    case view dbDSL of
        Return a -> return a
        a :>>= nextStep -> go a nextStep
  where
    go :: DbAction a -> (a -> DbDSL b) -> SqlPersistT (EitherT ServantErr IO) b
    go (GetDb key) nextStep = do
        maybeVal <- get key
        runDbDSLInPersistent $ nextStep maybeVal
    go (InsertDb val) nextStep = do
        key <- insert val
        runDbDSLInPersistent $ nextStep key
    go (DelDb key) nextStep = do
        delete key
        runDbDSLInPersistent $ nextStep ()
    go (GetByDb uniqueVal) nextStep = do
        maybeEntityVal <- getBy uniqueVal
        runDbDSLInPersistent $ nextStep maybeEntityVal
    go (UpdateDb key val) nextStep = do
        replace key val
        runDbDSLInPersistent $ nextStep ()
    go (ThrowDb servantErr@(ServantErr httpStatusCode httpStatusString _ _)) _ = do
        -- In actual usage, you may need to rollback the database
        -- connection here.  It doesn't matter for this simple
        -- demonstration, but in production you'll probably want to roll
        -- back the current transaction when you use 'Throw'.
        -- conn <- ask
        -- liftIO $ connRollback conn (getStmtConn conn)
        liftIO $ putStrLn $
            "error occured: " ++ show (httpStatusCode,httpStatusString)
        throwM servantErr

-----------------
-- servant api --
-----------------

type CRUD a =                         ReqBody '[JSON] a :> Post '[JSON] (Key a) -- create
         :<|> Capture "id" (Key a)                      :> Get '[JSON] a -- read
         :<|> Capture "id" (Key a) :> ReqBody '[JSON] a :> Put '[JSON] () -- update
         :<|> Capture "id" (Key a)                      :> Delete '[JSON] () -- delete

-------------------------------------------

type DbDSLInterpreterServant a = DbDSL a -> EitherT ServantErr IO a

runCreateBlogPost :: DbDSLInterpreterServant (Key BlogPost)
                  -> BlogPost
                  -> EitherT ServantErr IO (Key BlogPost)
runCreateBlogPost interpreter val = interpreter $ insertDb val

runReadBlogPost :: DbDSLInterpreterServant BlogPost
                -> Key BlogPost
                -> EitherT ServantErr IO BlogPost
runReadBlogPost interpreter key = interpreter $ getOr404Db key

runUpdateBlogPost :: DbDSLInterpreterServant ()
                  -> Key BlogPost
                  -> BlogPost
                  -> EitherT ServantErr IO ()
runUpdateBlogPost interpreter key val = interpreter $ updateDb key val

runDeleteBlogPost :: DbDSLInterpreterServant ()
                  -> Key BlogPost
                  -> EitherT ServantErr IO ()
runDeleteBlogPost interpreter key = interpreter $ deleteDb key

-- Servant HTTP handlers for BlogPost CRUD requests.
server :: (forall a . DbDSLInterpreterServant a)
       -> Server ("blogpost" :> CRUD BlogPost)
server interpreter = runCreateBlogPost interpreter
                :<|> runReadBlogPost interpreter
                :<|> runUpdateBlogPost interpreter
                :<|> runDeleteBlogPost interpreter

defaultMain :: IO ()
defaultMain =
    runStderrLoggingT $ withSqliteConn ":memory:" $ \conn -> do
        liftIO $ runSqlConn (runMigration migrateAll) conn
        liftIO $ putStrLn "\napi running on port 8080..."
        liftIO $ run 8080 $ serve myApiType $ server $ runDbDSLInServant conn
  where
    myApiType :: Proxy ( "blogpost" :> CRUD BlogPost )
    myApiType = Proxy

    runDbDSLInServant :: SqlBackend
                      -> DbDSL a
                      -> EitherT ServantErr IO a
    runDbDSLInServant conn dbDSL = runSqlConn (runDbDSLInPersistent dbDSL) conn
                                    `catch` \(err::ServantErr) -> throwError err

--- XXX: Hack.
instance Exception ServantErr

-- XXX: Hack.
instance (ToBackendKey SqlBackend a) => FromText (Key a) where
    fromText :: Text -> Maybe (Key a)
    fromText text = toSqlKey <$> fromText text

