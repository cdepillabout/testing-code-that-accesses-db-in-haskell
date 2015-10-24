
-- These are the tests for our api.  The only real interesting parts are
-- the 'DBAccess' instance and 'app' functions.

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

import Control.Monad.Catch (MonadThrow, catch)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
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

import Lib (BlogPost(..), DBAccess(..), blogPostApiProxy, server)

-- | This is our 'DBAccess' instance for these unit tests.  It's very similar
-- to the 'DBAccess' instance in "Lib", except that it doesn't actually access
-- a database.  Instead, it just uses an 'IntMap' to simulate a database.

-- The 'runDb' method takes an 'IORef' that references an 'IntMap' and our
-- 'DB' monad, and evaluates it in a Servant context (e.g.  the @'EitherT'
-- 'ServantErrr' IO@ monad).
--
-- It's using an 'IORef' to hold a tuple of the the 'IntMap' and 'Int'
-- (corresponding to the id count) for simplicity, but it could easily be
-- rewritten to use something like a 'State' monad instead of an IORef.
--
-- The 'Int' (corresponding to the id count) is simply the highest id of
-- something in the database.  Everytime we insert something we increase
-- it by 1.

-- | This is just a simple newtype wrapper for our 'IORef'.
newtype DBIORef = DBIORef { unDBIORef :: IORef (IntMap BlogPost, Int) }

-- | This is also a simple newtype wrapper for our DB Monad.  This is very
-- similar to Persistent's 'SqlPersistT' type.
newtype DB m a = DB (ReaderT DBIORef m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader DBIORef, MonadThrow)

-- | Here is our instance of 'DBAccess' for accessing the database in
-- production.  It pretty much just directly wraps the calls to Persistent.
testDB :: DBIORef -> DBAccess (DB IO)
testDB config = DBAccess { runDb = runDb' config
                         , getDb = getDb'
                         , insertDb = insertDb'
                         , deleteDb = deleteDb'
                         , updateDb = updateDb'
                         }
  where
    -- | Evaluate our 'DB' moonad in a Servant context (e.g.  the
    -- @'EitherT' 'ServantErrr' IO@ monad).
    runDb' :: DBIORef -> DB IO a -> EitherT ServantErr IO a
    runDb' dbIORef (DB readerT) =
        liftIO (runReaderT readerT dbIORef)
            `catch` \(err::ServantErr) -> throwError err

    getDb' :: Key BlogPost -> DB IO (Maybe BlogPost)
    getDb' key = do
        -- Get the 'IntMap' from the 'IORef'.
        (intMap, _) <- liftIO . readIORef . unDBIORef =<< ask
        -- Lookup the key of the 'BlogPost' in the 'IntMap' and return it.
        return $ IntMap.lookup (sqlKeyToInt key) intMap

    -- | Put a 'BlogPost' into the hashmap and return the 'Key'.
    insertDb' :: BlogPost -> DB IO (Key BlogPost)
    insertDb' blogPost = do
        (DBIORef dbRef) <- ask
        (intMap, idCounter) <- liftIO $ readIORef dbRef
        let newIntMap = IntMap.insert idCounter blogPost intMap
            newCounter = idCounter + 1
        liftIO $ writeIORef dbRef (newIntMap, newCounter)
        return $ intToSqlKey idCounter

    -- | Delete a 'BlogPost' from the hashmap.
    deleteDb' :: Key BlogPost -> DB IO ()
    deleteDb' key = do
        (DBIORef dbRef) <- ask
        (intMap, counter) <- liftIO $ readIORef dbRef
        let newIntMap = IntMap.delete (sqlKeyToInt key) intMap
        liftIO $ writeIORef dbRef (newIntMap, counter)

    -- | Overwrite a 'BlogPost' from the hashmap with a new value.
    updateDb' :: Key BlogPost -> BlogPost -> DB IO ()
    updateDb' key blogPost = do
        (DBIORef dbRef) <- ask
        (intMap, counter) <- liftIO $ readIORef dbRef
        let newIntMap = IntMap.insert (sqlKeyToInt key) blogPost intMap
        liftIO $ writeIORef dbRef (newIntMap, counter)

-- | Turn a 'Key' 'BlogPost' into an 'Int'.  This is for storing a 'Key'
-- 'BlogPost' in our 'IntMap'.
sqlKeyToInt :: Key BlogPost -> Int
sqlKeyToInt key = fromInteger . toInteger $ fromSqlKey key

-- | Opposite of 'sqlKeyToInt'.
intToSqlKey :: Int -> Key BlogPost
intToSqlKey int = toSqlKey . fromInteger $ toInteger int

-- | This creates a Wai 'Application'.
--
-- It just creates a new 'IORef' to our 'IntMap', and passes it to
-- 'server'.  It then uses the 'serve' function to create a Wai
-- 'Application'.
app :: IO Application
app = do
    -- Create an 'IORef' that references a tuple of an 'IntMap' and 'Int'.
    -- The 'IntMap' will be our database.  The 'Int' will be a count
    -- holding the highest id in the database.
    dbRef <- newIORef (IntMap.empty, 1)
    return . serve blogPostApiProxy . server $ testDB $ DBIORef dbRef

-- | These are our actual unit tests.  They should be relatively
-- straightforward.
--
-- This function is using 'app', which in turn uses our testing instance of
-- 'DBAccess'.
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

