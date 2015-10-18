-- Inspired by
-- https://hbtvl.wordpress.com/2015/06/28/servant-persistent-and-dsls.

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

----------------------------------
-- Persistent model definitions --
----------------------------------

-- This uses Persistent (a database library) to define a BlogPost data
-- type as well as it's corresponding database table.
--
-- If you've never used Persistent, you can think of it as defining the
-- following data types and sql statement. This is a vast simplification of
-- what it is actually doing, but it's good for a start:
--
-- data BlogPost = BlogPost { blogPostTitle :: Text
--                          , blogPostContent :: Text
--                          }
--
-- type Key BlogPost = Int
--
-- CREATE TABLE "blog_post" ("id" PRIMARY KEY,"title" VARCHAR,"content" VARCHAR)
--
-- It also defines some helper functions to query the "blog_post" table.
share [ mkPersist sqlSettings, mkMigrate "migrateAll"]
      [persistLowerCase|
BlogPost json
    title Text
    content Text
    deriving Show
|]

--------------------------------
-- DSL for accessing database --
--------------------------------

-- | The whole point of this "free-monad" example is the couple of lines
-- below.  We are defining a DSL that represents actions that can be
-- performed on a database.
--
-- A 'DbAction' is the /type/ of an action that can be performed on
-- a database.  For example, 'GetDb' is a data constructor that represents
-- getting a row of data from the database.  'InsertDb' is a data
-- constructor that represents putting a row of data into the database.
--
-- 'DbDSL' represents a sequence of 'DbAction's.  The magic starts to come
-- in later on in this file.  We will construct an /interpreter/ that takes
-- a 'DbDSL' and actually performs the actions against a real database.
-- Then, in testing, we will write a /different interpreter/ that just
-- operates on a hashmap in memory.  It never actually operates on a real
-- database.
--
-- PROTIP: The following is not actually a free-monad, but is instead using
-- the operational monad.  In practice, it's not a huge difference, but it
-- is something to be aware of.
type DbDSL = Program DbAction
data DbAction a where
    ThrowDb  :: ServantErr -> DbAction a
    GetDb    :: Key BlogPost -> DbAction (Maybe BlogPost)
    InsertDb :: BlogPost -> DbAction (Key BlogPost)
    UpdateDb :: Key BlogPost -> BlogPost -> DbAction ()
    DelDb    :: Key BlogPost -> DbAction ()

-- The following helper functions make it easy to create a 'DbDSL'.  The
-- most interesting one is 'getOr404Db', which combines both 'getDb' and
-- 'throwDb' into one 'DbDSL'.

-- | Throws an error.
throwDb :: ServantErr -> DbDSL a
throwDb err = singleton (ThrowDb err)

-- | Get a 'BlogPost' from the database.
getDb :: Key BlogPost -> DbDSL (Maybe BlogPost)
getDb key = singleton (GetDb key)

-- | Insert a 'BlogPost' into the database and return its id.
insertDb :: BlogPost ->  DbDSL (Key BlogPost)
insertDb blogPost = singleton (InsertDb blogPost)

-- | Update a 'BlogPost' that already exists in the database.
updateDb :: Key BlogPost -> BlogPost -> DbDSL ()
updateDb key blogPost = singleton (UpdateDb key blogPost)

-- | Delete a 'BlogPost' from the database.
deleteDb :: Key BlogPost -> DbDSL ()
deleteDb key = singleton (DelDb key)

-- | Try to get a 'BlogPost' from the database.  If it doesn't exist, throw
-- an error.
getOr404Db :: Key BlogPost -> DbDSL BlogPost
getOr404Db key = do
    maybeVal <- getDb key
    case maybeVal of
        Just blogPost -> return blogPost
        Nothing -> throwDb err404

-----------------
-- servant api --
-----------------

-- | This defines a type which represents the API.  A description of the
-- API is given in the README.md.  If you read the README.md, this should
-- be very understandable.
type BlogPostApi = "create" :> ReqBody '[JSON] BlogPost
                            :> Post '[JSON] (Key BlogPost)

              :<|> "read"   :> Capture "id" (Key BlogPost)
                            :> Get '[JSON] BlogPost

              :<|> "update" :> Capture "id" (Key BlogPost)
                            :> ReqBody '[JSON] BlogPost
                            :> Put '[JSON] ()

              :<|> "delete" :> Capture "id" (Key BlogPost)
                            :> Delete '[JSON] ()

-- | This defines handlers for our API.  This 'server' function is
-- Servant-specfic and not too interesting.  If you want to learn more
-- about it, see the Servant tutorial.
--
-- The interesting thing is the 'createBlogPost', 'readBlogPost',
-- 'updateBlogPost', and 'deleteBlogPost' functions.  See their
-- documentation for an explanation of what they are doing.
server :: (forall a . DbDSL a -> EitherT ServantErr IO a) -> Server BlogPostApi
server interpreter = createBlogPost
                :<|> readBlogPost
                :<|> updateBlogPost
                :<|> deleteBlogPost
  where
    -- This is the handler for the API call that creates a blog post.
    --
    -- Looking at the type, you can see that we get a 'BlogPost' object as
    -- input, and we need to return a 'Key' 'BlogPost' (which you can think
    -- of as an integer that corresponds to a database id).
    --
    --
    createBlogPost :: BlogPost -> EitherT ServantErr IO (Key BlogPost)
    createBlogPost blogPost = interpreter $ insertDb blogPost

    readBlogPost :: Key BlogPost -> EitherT ServantErr IO BlogPost
    readBlogPost key = interpreter $ getOr404Db key

    updateBlogPost :: Key BlogPost -> BlogPost -> EitherT ServantErr IO ()
    updateBlogPost key val = interpreter $ updateDb key val

    deleteBlogPost :: Key BlogPost -> EitherT ServantErr IO ()
    deleteBlogPost key = interpreter $ deleteDb key

blogPostApiProxy :: Proxy BlogPostApi
blogPostApiProxy = Proxy




------------------------------
-- database dsl interpreter --
------------------------------

runDbDSLInServant :: SqlBackend
                  -> DbDSL a
                  -> EitherT ServantErr IO a
runDbDSLInServant conn dbDSL =
    runSqlConn (runDbDSLInPersistent dbDSL) conn
        `catch` \(err::ServantErr) -> throwError err
  where
    runDbDSLInPersistent :: DbDSL b -> SqlPersistT (EitherT ServantErr IO) b
    runDbDSLInPersistent dbDSL' =
        case view dbDSL' of
            Return a -> return a
            (GetDb key) :>>= nextStep -> do
                maybeVal <- get key
                runDbDSLInPersistent $ nextStep maybeVal
            (InsertDb blogPost) :>>= nextStep -> do
                key <- insert blogPost
                runDbDSLInPersistent $ nextStep key
            (DelDb key) :>>= nextStep -> do
                delete key
                runDbDSLInPersistent $ nextStep ()
            (UpdateDb key blogPost) :>>= nextStep -> do
                replace key blogPost
                runDbDSLInPersistent $ nextStep ()
            (ThrowDb servantErr) :>>= _ ->
                -- In actual usage, you may need to rollback the database
                -- connection here.  It doesn't matter for this simple
                -- demonstration, but in production you'll probably want to roll
                -- back the current transaction when you use 'Throw'.
                -- conn <- ask
                -- liftIO $ connRollback conn (getStmtConn conn)
                throwM servantErr

defaultMain :: IO ()
defaultMain =
    runStderrLoggingT $ withSqliteConn "production.sqlite" $ \conn -> do
        liftIO $ runSqlConn (runMigration migrateAll) conn
        liftIO $ putStrLn "\napi running on port 8080..."
        liftIO $ run 8080 $ serve blogPostApiProxy $ server $ runDbDSLInServant conn

--- XXX: Hack.
instance Exception ServantErr

-- XXX: Hack.
instance (ToBackendKey SqlBackend a) => FromText (Key a) where
    fromText :: Text -> Maybe (Key a)
    fromText text = toSqlKey <$> fromText text

