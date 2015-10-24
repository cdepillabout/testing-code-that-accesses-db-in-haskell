
-- This approach is very straightforward.  We just use two separate
-- databases.  We have one database for production, called
-- "production.sqlite", and one database for testing, called
-- "testing.sqlite".  The database name is specified in the 'main'
-- function.

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

module Lib (module Lib, EntityField(..)) where

import Control.Exception (Exception)
import Control.Monad.Catch (catch, throwM)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Either (EitherT)
import Data.Proxy (Proxy(..))
import Database.Persist
    ( Key, EntityField, ToBackendKey, delete, get, insert, replace )
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
-- However, there is one interesting things here.  The first is the
-- 'createBlogPost', 'readBlogPost', 'updateBlogPost', and 'deleteBlogPost'
-- functions.  See their documentation for an explanation of what they are
-- doing.

-- In production, the 'SqlBackend' argument will contain connection
-- information to access the production database, while in testing, the
-- 'SqlBackend' argument will contain connection information to access
-- a testing database.
server :: SqlBackend -> Server BlogPostApi
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
    -- -- We use the 'runDb' function defined below.
    createBlogPost :: BlogPost -> EitherT ServantErr IO (Key BlogPost)
    createBlogPost blogPost = runDb $ insert blogPost

    -- This is the handler for the API call that fetches a blog post from
    -- the database.  Return a 404 if the blog post can't be found.
    readBlogPost :: Key BlogPost -> EitherT ServantErr IO BlogPost
    readBlogPost key = runDb $ do
        maybeVal <- get key
        case maybeVal of
            Just blogPost -> return blogPost
            Nothing -> throwM err404

    -- Similar to 'createBlogPost'.
    updateBlogPost :: Key BlogPost -> BlogPost -> EitherT ServantErr IO ()
    updateBlogPost key val = runDb $ replace key val

    -- Similar to 'createBlogPost'.
    deleteBlogPost :: Key BlogPost -> EitherT ServantErr IO ()
    deleteBlogPost key = runDb $ delete key

    -- This is a small helper function for running a Persistent database
    -- action.  This is used in the four handlers above.
    runDb :: SqlPersistT IO a -> EitherT ServantErr IO a
    runDb query =
        liftIO (runSqlConn query conn)
            `catch` \(err::ServantErr) -> throwError err

-- | This is another artifact of Servant.  See the Servant tutorial or this
-- article I wrote about Servant for an overview of what this is:
-- <http://functor.tokyo/blog/2015-08-13-servant-type-families>
blogPostApiProxy :: Proxy BlogPostApi
blogPostApiProxy = Proxy

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
        liftIO . run 8080 . serve blogPostApiProxy $ server conn

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

