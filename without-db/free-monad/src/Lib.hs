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
----------------------------------
-- Persistent model definitions --
----------------------------------
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

----------------------------------
----------------------------------
-- DSL for accessing a database --
----------------------------------
----------------------------------

-- | The whole point of this "free-monad" example is the next couple of
-- lines.  We are defining a DSL that represents actions that can be
-- performed on a database.
--
-- A 'DbAction' is the /type/ of an action that can be performed on
-- a database.  For example, 'GetDb' is a data constructor that represents
-- getting a row of data from the database.  'InsertDb' is a data
-- constructor that represents putting a row of data into the database.
--
-- 'DbDSL' represents a sequence of 'DbAction's.  The magic starts to
-- happen later in this file.  We will construct an /interpreter/ that
-- takes a 'DbDSL' and actually performs the actions against a real
-- database.  Then, in testing, we will write a /different interpreter/
-- that just operates on a hashmap in memory.  It never actually operates
-- on a real database.
--
-- (PROTIP: The following is not actually a free-monad, but is instead using
-- the operational monad.  In practice, it's not a huge difference, but it
-- is something to be aware of.)
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
-----------------
-- servant api --
-----------------
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
-- However, there are two interesting things here.  The first is the
-- 'createBlogPost', 'readBlogPost', 'updateBlogPost', and 'deleteBlogPost'
-- functions.  See their documentation for an explanation of what they are
-- doing.
--
-- The second interesting thing is the 'interpreter' arguement.  The
-- 'interpreter' is a function that takes a 'DbDSL' and runs it in
-- a Servant context (that is, inside a @'EitherT' 'ServantErr' IO@ monad).
--
-- This is what is actually evaluating the dsl.  In production the
-- 'interpreter' will actually access the database.  It will put new
-- 'BlogPost's in the database and read existing 'BlogPost's from the
-- database.  In testing, the 'interpreter' will just use a Hashmap in
-- memory to simulate database access.
--
-- The cool thing is that this 'server' function doesn't have to change
-- between production and testing.  The only thing that will change is the
-- 'interpreter' function.
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
    -- We use 'interpreter' and pass it the dsl @'insertDb' blogPost@.
    -- This dsl corresponds to inserting a 'BlogPost'.  The 'interpreter'
    -- will execute this dsl in the Servant context (@'EitherT'
    -- 'ServantErr' IO@).
    createBlogPost :: BlogPost -> EitherT ServantErr IO (Key BlogPost)
    createBlogPost blogPost = interpreter $ insertDb blogPost

    -- Similar to 'createBlogPost'.
    readBlogPost :: Key BlogPost -> EitherT ServantErr IO BlogPost
    readBlogPost key = interpreter $ getOr404Db key

    -- Similar to 'createBlogPost'.
    updateBlogPost :: Key BlogPost -> BlogPost -> EitherT ServantErr IO ()
    updateBlogPost key val = interpreter $ updateDb key val

    -- Similar to 'createBlogPost'.
    deleteBlogPost :: Key BlogPost -> EitherT ServantErr IO ()
    deleteBlogPost key = interpreter $ deleteDb key

-- | This is another artifact of Servant.  See the Servant tutorial or this
-- article I wrote about Servant for an overview of what this is:
-- <http://functor.tokyo/blog/2015-08-13-servant-type-families>
blogPostApiProxy :: Proxy BlogPostApi
blogPostApiProxy = Proxy

------------------------------
------------------------------
-- database dsl interpreter --
------------------------------
------------------------------

-- | Remember the @interpreter@ argument for the 'server' function?  That's
-- basically what this function is.
--
-- If you curry the 'SqlBackend' argument, then you get a function @'DbDSL'
-- a -> 'EitherT' 'ServantErr' IO a@.  It takes a 'DbDSL' and evaluates it
-- in a Servant context (e.g. the @'EitherT' 'ServantErrr' IO@ monad).
--
-- The real interesting part is the 'runDbDSLInPersistent' helper
-- function.  It runs a 'DbDSL' in a persistent context (e.g. the
-- @'SqlPersistT' ('EitherT' 'ServantErr' IO)@ monad).  It actually
-- accesses the database.  It uses functions provided by the Persistent
-- library, for example, 'get', 'insert', 'replace'.
runDbDSLInServant :: SqlBackend
                  -> DbDSL a
                  -> EitherT ServantErr IO a
runDbDSLInServant conn dbDSL =
    -- 'runSqlConn' takes sql connection info ('SqlBackend') and uses it to
    -- run an 'SqlPersistT' against a real database.  We catch 'ServantErr'
    -- and re-throw them in the @'EitherT' 'ServantErr'@ monad.
    runSqlConn (runDbDSLInPersistent dbDSL) conn
        `catch` \(err::ServantErr) -> throwError err
  where
    -- | This takes a 'DbDSL' and runs it in a persistent context (e.g. the
    -- @'SqlPersistT' ('EitherT' 'ServantErr' IO)@ monad).  It actually
    -- accesses the database.
    --
    -- It works by pattern-matching on the dsl, using some machinery from
    -- the "Control.Monad.Operational" module.  Check out that module for
    -- an explanation of how it works.
    --
    -- Everything other than 'ThrowDb' calls 'runDbDSLInPersistent'
    -- recursively with the next step of the dsl.
    runDbDSLInPersistent :: DbDSL b -> SqlPersistT (EitherT ServantErr IO) b
    runDbDSLInPersistent dbDSL' =
        case view dbDSL' of
            Return a -> return a
            -- This evaluates a 'GetDb' request to actually get
            -- a 'BlogPost' from the database.
            (GetDb key) :>>= nextStep -> do
                -- 'get' is a function from Persistent that gets
                -- a 'BlogPost' from the database given a key.
                maybeVal <- get key
                runDbDSLInPersistent $ nextStep maybeVal
            -- Evaluate a 'InsertDb' request to insert a 'BlogPost' in to the
            -- database.
            (InsertDb blogPost) :>>= nextStep -> do
                key <- insert blogPost
                runDbDSLInPersistent $ nextStep key
            -- Evaluate a 'DelDb' request to delete a 'BlogPost' from the
            -- database.
            (DelDb key) :>>= nextStep -> do
                delete key
                runDbDSLInPersistent $ nextStep ()
            -- Evaluate a 'UpdateDb request to update a 'BlogPost' in the
            -- database.
            (UpdateDb key blogPost) :>>= nextStep -> do
                replace key blogPost
                runDbDSLInPersistent $ nextStep ()
            -- Throw an error to indicate that something went wrong.
            (ThrowDb servantErr) :>>= _ ->
                -- In actual usage, you may need to rollback the database
                -- transaction here.  It doesn't matter for this simple
                -- demonstration, but in production you'll probably want to roll
                -- back the current transaction when you use 'Throw'.
                -- conn <- ask
                -- liftIO $ connRollback conn (getStmtConn conn)
                throwM servantErr

----------
----------
-- main --
----------
----------

-- This is the main function.  It basically does three things.
--
--  1. Open up a connection to the sqlite database "production.sqlite".  In
--  production this would probably be something like Postgres, MongoDB,
--  AWS's DynamoDB, etc.
--  2. Perform migration.  This creates the "blog_post" table in the
--  database if it doesn't exist.
--  3. Run our 'server' function, which effectively runs the api.
defaultMain :: IO ()
defaultMain =
    runStderrLoggingT $ withSqliteConn "production.sqlite" $ \conn -> do
        liftIO $ runSqlConn (runMigration migrateAll) conn
        liftIO $ putStrLn "\napi running on port 8080..."
        liftIO $ run 8080 $ serve blogPostApiProxy $ server $ runDbDSLInServant conn


-----------------
-----------------
-- other stuff --
-----------------
-----------------

--- | XXX: Hack.
--
-- In the dsl interpreter, we @'throwM' 'ServantErr'@.  In order to use
-- 'ServantErr' with 'throwM', 'ServantErr' needs to be an instance of
-- 'Exception'.  In production code you probably don't want to do this.  It
-- makes this example code slightly simpler, but in actual code you
-- probably want to create your own exception type.
--
-- If you reuse 'ServantErr' like this you're creating an
-- <https://wiki.haskell.org/Orphan_instance orphan instance>.
instance Exception ServantErr

-- | XXX: Hack.
--
-- We need this to be able to read @'Key' 'BlogPost'@ from our api (for
-- example, in the "delete" api).  This instance gives us the ability to
-- create a @'Key' a@ from 'Text'.
--
-- This isn't bad, per se, but it needs UndecidableInstances to be able to
-- compile.  You can see
-- <https://hbtvl.wordpress.com/2015/06/28/servant-persistent-and-dsls this
-- blog post> on how to do something similar without having to use
-- UndecidableInstances.
instance (ToBackendKey SqlBackend a) => FromText (Key a) where
    fromText :: Text -> Maybe (Key a)
    fromText text = toSqlKey <$> fromText text

