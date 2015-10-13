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

module Lib where

import Control.Exception (Exception)
import Control.Lens
import Control.Monad
import Control.Monad.Catch (catch, throwM)
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Control.Monad.Operational as O
import Control.Monad.Operational hiding (view)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Int
import qualified Data.Foldable as F
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Text.Lens
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant

-- Inspired by
-- https://hbtvl.wordpress.com/2015/06/28/servant-persistent-and-dsls.

instance Exception ServantErr

----------------------------------
-- Persistent model definitions --
----------------------------------

share [ mkPersist sqlSettings { mpsGenerateLenses = True }
      , mkMigrate "migrateAll"]
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
throwDb = singleton . ThrowDb

-- | dual of `persistent`'s `get`
getDb :: PC val => Key val -> DbDSL (Maybe val)
getDb = singleton . GetDb

-- | dual of `persistent`'s `getBy`
getByDb :: PC val => Unique val ->  DbDSL (Maybe (Entity val))
getByDb = singleton . GetByDb

-- | dual of `persistent`'s `insert`
insertDb :: PC val => val ->  DbDSL (Key val)
insertDb = singleton . InsertDb

-- | dual of `persistent`'s `update`
updateDb :: PC val => Key val -> val -> DbDSL ()
updateDb k v = singleton (UpdateDb k v)

-- | dual of `persistent`'s `delete`
deleteDb :: PC val => Key val -> DbDSL ()
deleteDb = singleton . DelDb

-- | like `getDb` but throws a 404 if it could not find the corresponding record
getOr404Db :: PC val => Key val -> DbDSL val
getOr404Db = getDb >=> maybe (throwDb err404) return

-- | like `getByDb` but throws a 404 if it could not find the corresponding record
getByOr404Db :: PC val => Unique val -> DbDSL (Entity val)
getByOr404Db = getByDb >=> maybe (throwDb err404) return


runDbDSLInPersistent :: DbDSL a -> SqlPersistT (EitherT ServantErr IO) a
runDbDSLInPersistent ws =
    case O.view ws of
        Return a -> return a
        a :>>= f -> runM a f
  where
    runM :: DbAction a
         -> (a -> DbDSL b)
         -> SqlPersistT (EitherT ServantErr IO) b
    runM (GetDb key) f = do
        maybeVal <- get key
        runDbDSLInPersistent $ f maybeVal
    runM (InsertDb val) f = do
        key <- insert val
        runDbDSLInPersistent $ f key
    runM (DelDb key) f = do
        delete key
        runDbDSLInPersistent $ f ()
    runM (GetByDb uniqueVal) f = do
        maybeEntityVal <- getBy uniqueVal
        runDbDSLInPersistent $ f maybeEntityVal
    runM (UpdateDb key val) f = do
        replace key val
        runDbDSLInPersistent $ f ()
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
