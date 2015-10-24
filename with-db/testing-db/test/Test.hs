
-- These are the tests for our api.  The only real interesting part is the
-- 'main' function, were we specific that the test database is different
-- from the production database.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT(..))
import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import Database.Persist ((>=.), deleteWhere)
import Database.Persist.Sql (toSqlKey)
import Database.Persist.Sqlite (runMigration, runSqlConn, withSqliteConn)
import Network.HTTP.Types.Method (methodPost, methodPut)
import Network.Wai (Application)
import Network.Wai.Test (SResponse)
import Servant.Server (serve)
import Test.Hspec (Spec, describe, hspec, it)
import Test.Hspec.Wai
    ( WaiExpectation, WaiSession, delete, get, matchBody, request
    , shouldRespondWith, with )

import Lib (BlogPost(..), EntityField(..), blogPostApiProxy, migrateAll, server)

-- | These are our actual unit tests.  They should be relatively
-- straightforward.
--
-- This function is using 'app', which in turn accesses our testing
-- database.
spec :: IO Application -> Spec
spec app = with app $ do
    describe "GET blogpost" $ do

        it "responds with 200 after inserting something" $ do
            postJson "/create" testBlogPost `shouldRespondWith` 201
            get "/read/1" `shouldRespondWithJson` (200, testBlogPost)
        it "responds with 404 because nothing has been inserted" $ do
            get "/read/1" `shouldRespondWith` 404

    describe "PUT blogpost" $ do
        it "responds with 204 even when key doesn't exist in DB" $ do
            putJson "/update/1" testBlogPost `shouldRespondWith` 204

        it "can't GET after PUT" $ do
            putJson "/update/1" testBlogPost `shouldRespondWith` 204
            get "/read/1" `shouldRespondWith` 404

    describe "DELETE blogpost" $ do
        it "responds with 204 even when key doesn't exist in DB" $ do
            delete "/delete/1" `shouldRespondWith` 204

        it "GET after DELETE returns 404" $ do
            postJson "/create" testBlogPost `shouldRespondWith` 201
            get "/read/1" `shouldRespondWith` 200
            delete "/delete/1" `shouldRespondWith` 204
            get "/read/1" `shouldRespondWith` 404
  where
    -- Send a type that can be turned into JSON (@a@) to the Wai
    -- 'Application' at the 'ByteString' url.  This returns a 'SResponse'
    -- in the 'WaiSession' monad.  This is similar to the 'post' function.
    postJson :: (ToJSON a) => ByteString -> a -> WaiSession SResponse
    postJson path =
        request methodPost path [("Content-Type", "application/json")] . encode

    -- Similar to 'postJson'.
    putJson :: (ToJSON a) => ByteString -> a -> WaiSession SResponse
    putJson path =
        request methodPut path [("Content-Type", "application/json")] . encode

    -- Similar to 'shouldRespondWith', but converts the second argument to
    -- JSON before it compares with the 'SResponse'.
    shouldRespondWithJson :: (ToJSON a)
                          => WaiSession SResponse
                          -> (Integer, a)
                          -> WaiExpectation
    shouldRespondWithJson req (expectedStatus, expectedValue) =
        let matcher = (fromInteger expectedStatus)
                        { matchBody = Just $ encode expectedValue }
        in shouldRespondWith req matcher

    -- An example blog post to use in tests.
    testBlogPost :: BlogPost
    testBlogPost = BlogPost "title" "content"

-- | This is almost identical to the 'defaultMain' defined in "Lib", except
-- that is it running against "testing.sqlite" instead of
-- "production.sqlite".
main :: IO ()
main =
    runNoLoggingT $ withSqliteConn "testing.sqlite" $ \conn -> do
        liftIO $ runSqlConn (runMigration migrateAll) conn
        liftIO $ putStrLn "\napi running on port 8080..."
        liftIO $ hspec $ spec $ do
            -- Before running each test, we have to remove all of the
            -- existing blog posts from the database.  This ensures that
            -- it doesn't matter which order the tests are run in.
            runSqlConn (deleteWhere [BlogPostId >=. toSqlKey 0]) conn
            return . serve blogPostApiProxy $ server conn

