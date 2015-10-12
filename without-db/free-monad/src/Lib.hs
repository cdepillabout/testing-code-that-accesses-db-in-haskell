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


-----------------
-- servant api --
-----------------

type CRUD a =                         ReqBody '[JSON] a :> Post '[JSON] (Key a) -- create
         :<|> Capture "id" (Key a)                      :> Get '[JSON] a -- read
         :<|> Capture "id" (Key a) :> ReqBody '[JSON] a :> Put '[JSON] () -- update
         :<|> Capture "id" (Key a)                      :> Delete '[JSON] () -- delete

type MyApi = "author" :> CRUD Author
        :<|> "post"   :> CRUD BlogPost

myApi :: Proxy MyApi
myApi = Proxy

-- XXX: Hack.
instance (ToBackendKey SqlBackend a) => FromText (Key a) where
    fromText :: Text -> Maybe (Key a)
    fromText text = toSqlKey <$> fromText text

-------------------------
-- DSL for persistent? --
-------------------------

type WebService = Program WebAction
type PC val = (PersistEntityBackend val ~ SqlBackend, PersistEntity val)
data WebAction a where
    Throw :: ServantErr               -> WebAction a
    Get   :: PC val => Key val        -> WebAction (Maybe val)
    Del   :: PC val => Key val        -> WebAction ()
    GetBy :: PC val => Unique val     -> WebAction (Maybe (Entity val))
    New   :: PC val =>            val -> WebAction (Key val)
    Upd   :: PC val => Key val -> val -> WebAction ()

-- | throws an error
throw :: ServantErr -> WebService a
throw = singleton . Throw

-- | dual of `persistent`'s `get`
mget :: PC val => Key val -> WebService (Maybe val)
mget = singleton . Get

-- | dual of `persistent`'s `getBy`
mgetBy :: PC val => Unique val ->  WebService (Maybe (Entity val))
mgetBy = singleton . GetBy

-- | dual of `persistent`'s `insert`
mnew :: PC val => val ->  WebService (Key val)
mnew = singleton . New

-- | dual of `persistent`'s `update`
mupd :: PC val => Key val -> val -> WebService ()
mupd k v = singleton (Upd k v)

-- | dual of `persistent`'s `delete`
mdel :: PC val => Key val -> WebService ()
mdel = singleton . Del

-- | like `mget` but throws a 404 if it could not find the corresponding record
mgetOr404 :: PC val => Key val -> WebService val
mgetOr404 = mget >=> maybe (throw err404) return

-- | like `mgetBy` but throws a 404 if it could not find the corresponding record
mgetByOr404 :: PC val => Unique val -> WebService (Entity val)
mgetByOr404 = mgetBy >=> maybe (throw err404) return


runDbDSL :: WebService a -> SqlPersistT (EitherT ServantErr IO) a
runDbDSL ws = case O.view ws of
                  Return a -> return a
                  a :>>= f -> runM a f
  where
    runM :: WebAction a -> (a -> WebService b) -> SqlPersistT (EitherT ServantErr IO) b
    runM (Get key) f = do
        maybeVal <- get key
        runDbDSL $ f maybeVal
    runM (New val) f = do
        key <- insert val
        runDbDSL $ f key
    runM (Del key) f = do
        delete key
        runDbDSL $ f ()
    runM (GetBy uniqueVal) f = do
        maybeEntityVal <- getBy uniqueVal
        runDbDSL $ f maybeEntityVal
    runM (Upd key val) f = do
        replace key val
        runDbDSL $ f ()
    runM (Throw servantErr@(ServantErr httpStatusCode httpStatusString _ _)) _ = do
        -- XXX: Don't need to rollback in the case of things going wrong?
        -- Is this handled automatically?
        -- conn <- ask
        -- liftIO $ connRollback conn (getStmtConn conn)
        liftIO $ putStrLn $ "error occured: " ++ show (httpStatusCode,httpStatusString)
        throwM servantErr

runServant :: forall a . (PersistEntity a, ToBackendKey SqlBackend a)
        => SqlBackend -- ^ Connection pool
        ->
            ( (a          -> EitherT ServantErr IO (Key a))
         :<|> (Key a      -> EitherT ServantErr IO a      )
         :<|> (Key a -> a -> EitherT ServantErr IO ()     )
         :<|> (Key a      -> EitherT ServantErr IO ()     )
            )
runServant conn =
    runnew :<|> runget :<|> runupd :<|> rundel
  where
    runnew :: a -> EitherT ServantErr IO (Key a)
    runnew val = runQuery conn $ mnew val

    runget :: Key a -> EitherT ServantErr IO a
    runget key = runQuery conn $ mgetOr404 key

    runupd :: Key a -> a -> EitherT ServantErr IO ()
    runupd key val = runQuery conn $ mupd key val

    rundel :: Key a -> EitherT ServantErr IO ()
    rundel key = runQuery conn $ mdel key

runQuery :: forall a . SqlBackend -> WebService a -> EitherT ServantErr IO a
runQuery conn webservice = runSqlConn (runDbDSL webservice) conn
                                `catch` \(err::ServantErr) -> throwError err

server :: SqlBackend -> Server MyApi
server conn = runServant conn
         :<|> runServant conn

defaultMain :: IO ()
defaultMain =
    runStderrLoggingT $ withSqliteConn ":memory:" $ \conn -> do
        liftIO $ runSqlConn (runMigration migrateAll) conn
        liftIO $ putStrLn "\napi running on port 8080..."
        liftIO $ run 8080 (serve myApi (server conn))
