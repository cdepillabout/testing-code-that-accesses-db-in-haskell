
-- These are the tests for our api.  The only real interesting parts are
-- the 'testDbDSLInServant' and 'app' functions.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Operational (ProgramViewT(..), view)
import Control.Monad.Trans.Either (EitherT)
import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import Data.IntMap.Lazy (IntMap)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Lazy as IntMap
import Database.Persist (Key)
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Network.HTTP.Types.Method (methodPost, methodPut)
import Network.Wai (Application)
import Network.Wai.Test (SResponse)
import Servant.Server (ServantErr(..), serve)
import Test.Hspec (Spec, describe, hspec, it)
import Test.Hspec.Wai
    ( WaiExpectation, WaiSession, delete, get, matchBody, request
    , shouldRespondWith, with )

import Lib (BlogPost(..), DbAction(..), DbDSL, blogPostApiProxy, server)

-- | This is our dsl interpreter for these unit tests.  It's very similar
-- to 'Lib.runDbDSLInServant', except that it doesn't actually access
-- a database.  Instead, it just uses an 'IntMap' to simulate a database.

-- It's similar to 'runDbDSLInServant' in that if you curry the 'IORef'
-- argument, then you get a function @'DbDSL' a -> 'EitherT' 'ServantErr'
-- IO a@.  It takes a 'DbDSL' and evaluates it in a Servant context (e.g.
-- the @'EitherT' 'ServantErrr' IO@ monad).
--
-- It's using an 'IORef' to hold a tuple of the the 'IntMap' and 'Int'
-- corresponding to the id count for simplicity, but it could easily be
-- rewritten to use something like a 'State' monad.
--
-- The 'Int' corresponding to the id count is simply the highest id of
-- something in the database.  Everytime we insert something we increase
-- it by 1.
testDbDSLInServant :: IORef (IntMap BlogPost, Int)
                   -> DbDSL a
                   -> EitherT ServantErr IO a
testDbDSLInServant dbRef dbDSL = do
    case view dbDSL of
        Return a -> return a
        -- This evaluates a 'GetDb' request to actually get
        -- a 'BlogPost' from the hashmap.
        (GetDb key) :>>= nextStep -> do
            -- Get the 'IntMap' from the 'IORef'.
            (intMap, _) <- liftIO $ readIORef dbRef
            -- Lookup the key of the 'BlogPost' in the 'IntMap'.
            let maybeBlogPost = IntMap.lookup (sqlKeyToInt key) intMap
            -- Run the next step of the dsl, passing it the 'BlogPost'.
            testDbDSLInServant dbRef $ nextStep maybeBlogPost
        -- Evaluate a 'InsertDb' request to insert a 'BlogPost' in to the
        -- hashmap.
        (InsertDb blogPost) :>>= nextStep -> do
            (intMap, idCounter) <- liftIO $ readIORef dbRef
            let newIntMap = IntMap.insert idCounter blogPost intMap
                newCounter = idCounter + 1
            liftIO $ writeIORef dbRef (newIntMap, newCounter)
            testDbDSLInServant dbRef . nextStep $ intToSqlKey idCounter
        -- Evaluate a 'DelDb' request to delete a 'BlogPost' from the
        -- hashmap.
        (DelDb key) :>>= nextStep -> do
            (intMap, counter) <- liftIO $ readIORef dbRef
            let newIntMap = IntMap.delete (sqlKeyToInt key) intMap
            liftIO $ writeIORef dbRef (newIntMap, counter)
            testDbDSLInServant dbRef $ nextStep ()
        -- Evaluate an 'UpdateDb' request to update a 'BlogPost' in the
        -- hashmap.
        (UpdateDb key blogPost) :>>= nextStep -> do
            (intMap, counter) <- liftIO $ readIORef dbRef
            let newIntMap = IntMap.insert (sqlKeyToInt key) blogPost intMap
            liftIO $ writeIORef dbRef (newIntMap, counter)
            testDbDSLInServant dbRef $ nextStep ()
        -- Throw an error to indicate that something went wrong.
        (ThrowDb servantErr) :>>= _ ->
            throwError servantErr
  where
    sqlKeyToInt :: Key BlogPost -> Int
    sqlKeyToInt key = fromInteger . toInteger $ fromSqlKey key

    intToSqlKey :: Int -> Key BlogPost
    intToSqlKey int = toSqlKey . fromInteger $ toInteger int

-- | This creates a Wai 'Application'.
--
-- It just creates a new 'IORef' to our 'IntMap', and passes it to
-- 'testDbDSLInServant'.  It then uses the 'serve' function to create a Wai
-- 'Application'.
app :: IO Application
app = do
    -- Create an 'IORef' to a tuple of an 'IntMap' and 'Int'.
    -- The 'IntMap' will be our database.  The 'Int' will be a count
    -- holding the highest id in the database.
    dbRef <- newIORef (IntMap.empty, 1)
    return . serve blogPostApiProxy $ server (testDbDSLInServant dbRef)

-- | These are our actual unit tests.  They should be relatively
-- straightforward.
--
-- This function is using 'app', which in turn uses our test dsl interpreter
-- ('testDbDSLInServant').
spec :: Spec
spec = with app $ do
    describe "GET blogpost" $ do
        it "responds with 404 because nothing has been inserted" $ do
            get "/read/1" `shouldRespondWith` 404

        it "responds with 200 after inserting something" $ do
            postJson "/create" testBlogPost `shouldRespondWith` 201
            get "/read/1" `shouldRespondWithJson` (200, testBlogPost)

    describe "PUT blogpost" $ do
        it "responds with 204 even when key doesn't exist in DB" $ do
            putJson "/update/1" testBlogPost `shouldRespondWith` 204

        it "can GET after PUT" $ do
            putJson "/update/1" testBlogPost `shouldRespondWith` 204
            get "/read/1" `shouldRespondWithJson` (200, testBlogPost)

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

main :: IO ()
main = hspec spec

