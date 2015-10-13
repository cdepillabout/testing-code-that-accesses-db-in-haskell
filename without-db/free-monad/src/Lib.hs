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
    Throw :: ServantErr               -> DbAction a
    Get   :: PC val => Key val        -> DbAction (Maybe val)
    Del   :: PC val => Key val        -> DbAction ()
    GetBy :: PC val => Unique val     -> DbAction (Maybe (Entity val))
    New   :: PC val =>            val -> DbAction (Key val)
    Upd   :: PC val => Key val -> val -> DbAction ()

-- | throws an error
throw :: ServantErr -> DbDSL a
throw = singleton . Throw

-- | dual of `persistent`'s `get`
mget :: PC val => Key val -> DbDSL (Maybe val)
mget = singleton . Get

-- | dual of `persistent`'s `getBy`
mgetBy :: PC val => Unique val ->  DbDSL (Maybe (Entity val))
mgetBy = singleton . GetBy

-- | dual of `persistent`'s `insert`
mnew :: PC val => val ->  DbDSL (Key val)
mnew = singleton . New

-- | dual of `persistent`'s `update`
mupd :: PC val => Key val -> val -> DbDSL ()
mupd k v = singleton (Upd k v)

-- | dual of `persistent`'s `delete`
mdel :: PC val => Key val -> DbDSL ()
mdel = singleton . Del

-- | like `mget` but throws a 404 if it could not find the corresponding record
mgetOr404 :: PC val => Key val -> DbDSL val
mgetOr404 = mget >=> maybe (throw err404) return

-- | like `mgetBy` but throws a 404 if it could not find the corresponding record
mgetByOr404 :: PC val => Unique val -> DbDSL (Entity val)
mgetByOr404 = mgetBy >=> maybe (throw err404) return


runDbDSL :: DbDSL a -> SqlPersistT (EitherT ServantErr IO) a
runDbDSL ws =
    case O.view ws of
        Return a -> return a
        a :>>= f -> runM a f
  where
    runM :: DbAction a
         -> (a -> DbDSL b)
         -> SqlPersistT (EitherT ServantErr IO) b
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
runCreateRest conn val = runQuery conn $ mnew val

runReadRest :: (PersistEntity a, ToBackendKey SqlBackend a)
       => SqlBackend
       -> Key a
       -> EitherT ServantErr IO a
runReadRest conn key = runQuery conn $ mgetOr404 key

runUpdateRest :: (PersistEntity a, ToBackendKey SqlBackend a)
       => SqlBackend
       -> Key a
       -> a
       -> EitherT ServantErr IO ()
runUpdateRest conn key val = runQuery conn $ mupd key val

runDeleteRest :: (PersistEntity a, ToBackendKey SqlBackend a)
       => SqlBackend
       -> Key a
       -> EitherT ServantErr IO ()
runDeleteRest conn key = runQuery conn $ mdel key

runQuery :: forall a . SqlBackend -> DbDSL a -> EitherT ServantErr IO a
runQuery conn dbDSL = runSqlConn (runDbDSL dbDSL) conn
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
