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
    ( Key, ToBackendKey, delete, get, insert, replace )
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
data DbAction a where
    ThrowDb  :: ServantErr -> DbAction a
    GetDb    :: Key BlogPost -> DbAction (Maybe BlogPost)
    InsertDb :: BlogPost -> DbAction (Key BlogPost)
    UpdateDb :: Key BlogPost -> BlogPost -> DbAction ()
    DelDb    :: Key BlogPost -> DbAction ()

-- | throws an error
throwDb :: ServantErr -> DbDSL a
throwDb err = singleton (ThrowDb err)

-- | dual of `persistent`'s `get`
getDb :: Key BlogPost -> DbDSL (Maybe BlogPost)
getDb key = singleton (GetDb key)

-- | dual of `persistent`'s `insert`
insertDb :: BlogPost ->  DbDSL (Key BlogPost)
insertDb blogPost = singleton (InsertDb blogPost)

-- | dual of `persistent`'s `update`
updateDb :: Key BlogPost -> BlogPost -> DbDSL ()
updateDb key blogPost = singleton (UpdateDb key blogPost)

-- | dual of `persistent`'s `delete`
deleteDb :: Key BlogPost -> DbDSL ()
deleteDb key = singleton (DelDb key)

-- | like `getDb` but throws a 404 if it could not find the corresponding record
getOr404Db :: Key BlogPost -> DbDSL BlogPost
getOr404Db key = do
    maybeVal <- getDb key
    case maybeVal of
        Just blogPost -> return blogPost
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
    go (InsertDb blogPost) nextStep = do
        key <- insert blogPost
        runDbDSLInPersistent $ nextStep key
    go (DelDb key) nextStep = do
        delete key
        runDbDSLInPersistent $ nextStep ()
    go (UpdateDb key blogPost) nextStep = do
        replace key blogPost
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

runDbDSLInServant :: SqlBackend
                  -> DbDSL a
                  -> EitherT ServantErr IO a
runDbDSLInServant conn dbDSL =
    runSqlConn (runDbDSLInPersistent dbDSL) conn
        `catch` \(err::ServantErr) -> throwError err

-----------------
-- servant api --
-----------------

type MyApi = "create" :> ReqBody '[JSON] BlogPost
                      :> Post '[JSON] (Key BlogPost)
        :<|> "read" :> Capture "id" (Key BlogPost)
                    :> Get '[JSON] BlogPost
        :<|> "update" :> Capture "id" (Key BlogPost)
                      :> ReqBody '[JSON] BlogPost
                      :> Put '[JSON] ()
        :<|> "delete" :> Capture "id" (Key BlogPost)
                      :> Delete '[JSON] ()
-------------------------------------------

type DbDSLInterpreterServant a = DbDSL a -> EitherT ServantErr IO a

createBlogPost :: DbDSLInterpreterServant (Key BlogPost)
                  -> BlogPost
                  -> EitherT ServantErr IO (Key BlogPost)
createBlogPost interpreter val = interpreter $ insertDb val

readBlogPost :: DbDSLInterpreterServant BlogPost
                -> Key BlogPost
                -> EitherT ServantErr IO BlogPost
readBlogPost interpreter key = interpreter $ getOr404Db key

updateBlogPost :: DbDSLInterpreterServant ()
                  -> Key BlogPost
                  -> BlogPost
                  -> EitherT ServantErr IO ()
updateBlogPost interpreter key val = interpreter $ updateDb key val

deleteBlogPost :: DbDSLInterpreterServant ()
                  -> Key BlogPost
                  -> EitherT ServantErr IO ()
deleteBlogPost interpreter key = interpreter $ deleteDb key

-- Servant HTTP handlers for BlogPost CRUD requests.
server :: (forall a . DbDSLInterpreterServant a) -> Server MyApi
server interpreter = createBlogPost interpreter
                :<|> readBlogPost interpreter
                :<|> updateBlogPost interpreter
                :<|> deleteBlogPost interpreter

myApiType :: Proxy ( "blogpost" :> MyApi )
myApiType = Proxy

defaultMain :: IO ()
defaultMain =
    runStderrLoggingT $ withSqliteConn ":memory:" $ \conn -> do
        liftIO $ runSqlConn (runMigration migrateAll) conn
        liftIO $ putStrLn "\napi running on port 8080..."
        liftIO $ run 8080 $ serve myApiType $ server $ runDbDSLInServant conn

--- XXX: Hack.
instance Exception ServantErr

-- XXX: Hack.
instance (ToBackendKey SqlBackend a) => FromText (Key a) where
    fromText :: Text -> Maybe (Key a)
    fromText text = toSqlKey <$> fromText text

