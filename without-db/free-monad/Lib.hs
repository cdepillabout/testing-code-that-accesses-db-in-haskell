{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Lib where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Control.Monad.Operational  as O
import           Control.Monad.Operational  hiding (view)
import           Control.Monad.Reader       (ask)
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Int
import qualified Data.Foldable              as F
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Text.Lens
import           Data.Text                  (Text)
import qualified Network.Wai.Handler.Warp
import           Servant

data AccessType = NoAccess | ReadOnly | ReadWrite | Owner

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

type CRUD a = ReqBody '[JSON] a :> Post '[JSON] (Key a) -- create
         :<|> Capture "id" (Key a) :> Get '[JSON] a -- read
         :<|> Capture "id" (Key a) :> ReqBody '[JSON] a :> Put '[JSON] () -- update
         :<|> Capture "id" (Key a) :> Delete '[JSON] () -- delete

type MyApi = "author" :> CRUD Author
        :<|> "post"   :> CRUD BlogPost

myApi :: Proxy MyApi
myApi = Proxy


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

-- throws an error
throw :: ServantErr -> WebService a
throw = singleton . Throw

-- dual of `persistent`'s `get`
mget :: PC val => Key val -> WebService (Maybe val)
mget = singleton . Get

-- dual of `persistent`'s `getBy`
mgetBy :: PC val => Unique val ->  WebService (Maybe (Entity val))
mgetBy = singleton . GetBy

-- dual of `persistent`'s `insert`
mnew :: PC val => val ->  WebService (Key val)
mnew = singleton . New

-- dual of `persistent`'s `update`
mupd :: PC val => Key val -> val -> WebService ()
mupd k v = singleton (Upd k v)

-- dual of `persistent`'s `delete`
mdel :: PC val => Key val -> WebService ()
mdel = singleton . Del

-- like `mget` but throws a 404 if it could not find the corresponding record
mgetOr404 :: PC val => Key val -> WebService val
mgetOr404 = mget >=> maybe (throw err404) return

-- like `mgetBy` but throws a 404 if it could not find the corresponding record
mgetByOr404 :: PC val => Unique val -> WebService (Entity val)
mgetByOr404 = mgetBy >=> maybe (throw err404) return


type ServantIO a = SqlPersistT (LoggingT (EitherT ServantErr IO)) a



runServant :: WebService a -> ServantIO a
runServant ws = case O.view ws of
                  Return a -> return a
                  a :>>= f -> runM a f

runM :: WebAction a -> (a -> WebService b) -> ServantIO b
runM x f = case x of
    Throw rr@(ServantErr c rs _ _) -> do
                  conn <- ask
                  liftIO $ connRollback conn (getStmtConn conn)
                  logOtherNS "WS" LevelError (show (c,rs) ^. packed)
                  throwError rr
    Get k    -> get k       >>= runServant . f
    New v    -> insert v    >>= runServant . f
    Del v    -> delete v    >>= runServant . f
    GetBy u  -> getBy u     >>= runServant . f
    Upd k v  -> replace k v >>= runServant . f


runCrud :: (PersistEntity a, ToBackendKey SqlBackend a, PC b)
        => ConnectionPool -- ^ Connection pool
        -> PermsFor a -- ^ Permission checking record
        -> Maybe (Key Person -> Key a -> AccessType -> b) -- ^ Extra actions after creation
        -> Maybe (Key a -> WebService ()) -- ^ Extra actions after deletion
        -> (      Maybe Text          -> a -> EitherT ServantErr IO (Key a))
           :<|> ((Maybe Text -> Key a      -> EitherT ServantErr IO a)
           :<|> ((Maybe Text -> Key a -> a -> EitherT ServantErr IO ())
           :<|> ( Maybe Text -> Key a      -> EitherT ServantErr IO ()))
           )
runCrud pool (PermsFor pnew pget pupd pdel) rightConstructor predelete =
          runnew :<|> runget :<|> runupd :<|> rundel
    where
        auth Nothing _ = throw err401
        auth (Just dn) perm = do
            user <- mgetBy (UniqueName dn) >>= maybe (throw err403) return
            check <- checkPerms user perm
            unless check (throw err403)
            return user
        runget dn mk = runQuery $ do
            let k = mk ^. _MKey
            void $ auth dn (pget k)
            mgetOr404 k
        runnew dn val = runQuery $ do
            usr <- auth dn pnew
            k <- mnew val
            F.mapM_ (\c -> mnew (c (entityKey usr) k Owner)) rightConstructor
            return (k ^. from _MKey)
        runupd dn mk val = runQuery $ do
            let k = mk ^. _MKey
            void $ auth dn (pupd k)
            mupd k val
        rundel dn mk = runQuery $ do
            let k = mk ^. _MKey
            void $ auth dn (pdel k)
            F.mapM_ ($ k) predelete
            mdel k
        runQuery :: WebService a -> EitherT ServantErr IO a
        runQuery ws = runStderrLoggingT $ runSqlPool (runServant ws) pool


-- A default action for when you need not run additional actions after creation
noCreateRightAdjustment :: Maybe (Key Person -> Key a -> AccessType -> PostRights)
noCreateRightAdjustment = Nothing

server :: ConnectionPool -> Server MyApi
server pool =
      runCrud pool adminOnly noCreateRightAdjustment Nothing
 :<|> defaultCrud blogPostRight PostRights Nothing
   where
     editRights c cid = rw (c cid) .|| isAdmin
     delRights c cid = owner (c cid) .|| isAdmin
     defaultPermissions c =
          PermsFor always
                  (const always)
                  (editRights c)
                  (delRights c)
     defaultCrud c r d = runCrud pool (defaultPermissions c)
                                 (Just r) d

defaultMain :: IO ()
defaultMain = do
  pool <- runStderrLoggingT $ do
      p <- createSqlitePool ":memory:" 1
      runSqlPool (runMigration migrateAll) p
      return p
  Network.Wai.Handler.Warp.run 8080 (serve myApi (server pool))
