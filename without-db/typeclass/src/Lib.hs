
-- This approach uses a free-monad to make a DSL to describe database
-- access.  Two separate interpreters for the DSL are created.  One
-- interpreter is used in production and one interpreter is used for tests.
-- The interpreter used in production actually interacts with the database
-- (e.g. putting data into the database and getting data out of the
-- database).  The interpreter used for tests simulates a database using a
-- hashmap.
--
-- This is heavily inspired by
-- https://hbtvl.wordpress.com/2015/06/28/servant-persistent-and-dsls.

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
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
import Control.Monad.Catch (MonadThrow, catch, throwM)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
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

----------------------------------------
----------------------------------------
-- Typeclass for accessing a database --
----------------------------------------
----------------------------------------

-- | The whole point of this "typeclass" example is the next couple of
-- lines.  We are defining a typeclass that represents actions that can be
-- performed on a database.
--
-- 'DBAccess' is our typeclass.  It represents types can be used to access
-- a DB (whether on disk, in memory, etc).  It takes two parameters, @m@
-- and @d@.
--
-- @m@ is the monad that we will be running in.  If you're doing something
-- like accessing a database, this might be 'IO'.  If you're using
-- Persistent it might be something like 'SqlPersistT' 'IO'.
--
-- @d@ is some data that needs to be passed in to actually run the database
-- requests.  If we are using persistent, it will probably be 'SqlBackend'.
--
-- 'getDb' is a function that lets us get a specific 'BlogPost' from the
-- database.  It is running in our @m@  monad.  The other functions are
-- similar.
--
-- 'runDb' actually lets us run our @m@ in a Servant context.
--
-- Later on in this file, we will define an instance of 'DBAccess' that
-- will allow us to access a Persistent database.  Then, in testing, we
-- will define a different instance of 'DBAccess' that allows us to access
-- an in-memory database modeled as a simple Hashmap.
--
-- (PROTIP1: Check out the argument to 'getDb': 'Key'.  'Key' is defined in
-- Persistent.  Ideally, this dsl would have no dependency on Persistent at
-- all.  I made the decision to have this dsl be dependent on Persistent in
-- order to simply the code and make it easier to understand.)

-- (PROTIP2: Modeling database access as a typeclass is very similar to how
-- Persistent itself works.)
class (MonadThrow m, Monad m) => DBAccess m d | m -> d, d -> m where

    runDb :: d -> m a -> EitherT ServantErr IO a

    getDb :: Key BlogPost -> m (Maybe BlogPost)

    insertDb :: BlogPost -> m (Key BlogPost)

    deleteDb :: Key BlogPost -> m ()

    updateDb :: Key BlogPost -> BlogPost -> m ()

-- | This tries to get a 'BlogPost' from our database, and throws an error
-- if it can't.
--
-- Helper functions like this can easily be written by using the 'DBAccess'
-- constraint.  The functions in 'DBAcesss' can be combined arbitrarily.
getOr404Db :: DBAccess m d => Key BlogPost -> m BlogPost
getOr404Db key = do
    maybeVal <- getDb key
    case maybeVal of
        Just blogPost -> return blogPost
        Nothing -> throwM err404

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

-- The second interesting thing is the @d@ arguement.  The
-- 'interpreter' is a function that takes a 'DbDSL' and runs it in
-- a Servant context (that is, inside a @'EitherT' 'ServantErr' IO@ monad).
--
-- The second interesting thing is the @d@ arguement.  The
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
server :: DBAccess m d => d -> Server BlogPostApi
server conn = createBlogPost
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
    createBlogPost blogPost = runDb conn $ insertDb blogPost

    -- Similar to 'createBlogPost'.
    readBlogPost :: Key BlogPost -> EitherT ServantErr IO BlogPost
    readBlogPost key = runDb conn $ getOr404Db key

    -- Similar to 'createBlogPost'.
    updateBlogPost :: Key BlogPost -> BlogPost -> EitherT ServantErr IO ()
    updateBlogPost key val = runDb conn $ updateDb key val

    -- Similar to 'createBlogPost'.
    deleteBlogPost :: Key BlogPost -> EitherT ServantErr IO ()
    deleteBlogPost key = runDb conn $ deleteDb key

-- | This is another artifact of Servant.  See the Servant tutorial or this
-- article I wrote about Servant for an overview of what this is:
-- <http://functor.tokyo/blog/2015-08-13-servant-type-families>
blogPostApiProxy :: Proxy BlogPostApi
blogPostApiProxy = Proxy

------------------------------
------------------------------
-- database typeclass instance --
------------------------------
------------------------------

-- | Here is our instance of 'DBAccess' for accessing the database in
-- production.  It pretty much just directly wraps the calls to Persistent.
instance DBAccess (SqlPersistT IO) SqlBackend where

    runDb :: SqlBackend -> SqlPersistT IO a -> EitherT ServantErr IO a
    runDb conn query =
        liftIO (runSqlConn query conn)
            `catch` \(err::ServantErr) -> throwError err

    getDb :: Key BlogPost -> SqlPersistT IO (Maybe BlogPost)
    getDb = get

    insertDb :: BlogPost -> SqlPersistT IO (Key BlogPost)
    insertDb = insert

    deleteDb :: Key BlogPost -> SqlPersistT IO ()
    deleteDb = delete

    updateDb :: Key BlogPost -> BlogPost -> SqlPersistT IO ()
    updateDb = replace

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
        liftIO $ run 8080 $ serve blogPostApiProxy $ server conn


-----------------
-----------------
-- other stuff --
-----------------
-----------------

--- | XXX: Hack.
--
-- Read the comment at the bottom of Lib.hs in the free-monad
-- implementation to find out more about this.
instance Exception ServantErr

-- | XXX: Hack.
--
-- Read the comment at the bottom of Lib.hs in the free-monad
-- implementation to find out more about this.
instance (ToBackendKey SqlBackend a) => FromText (Key a) where
    fromText :: Text -> Maybe (Key a)
    fromText text = toSqlKey <$> fromText text

