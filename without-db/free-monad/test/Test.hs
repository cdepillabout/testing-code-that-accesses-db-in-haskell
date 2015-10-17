{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.Error.Class (throwError)
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
import Test.Hspec
import Test.Hspec.Wai

import Lib

testDbDSLInServant :: IORef (IntMap BlogPost, Int)
                   -> DbDSL a
                   -> EitherT ServantErr IO a
testDbDSLInServant dbRef dbDSL = do
    case view dbDSL of
        Return a -> return a
        a :>>= nextStep -> go a nextStep
  where
    go :: DbAction a -> (a -> DbDSL b) -> EitherT ServantErr IO b
    go (GetDb key) nextStep = do
        (intMap, _) <- liftIO $ readIORef dbRef
        let maybeBlogPost = IntMap.lookup (sqlKeyToInt key) intMap
        testDbDSLInServant dbRef $ nextStep maybeBlogPost

    go (InsertDb blogPost) nextStep = do
        (intMap, idCounter) <- liftIO $ readIORef dbRef
        let newIntMap = IntMap.insert idCounter blogPost intMap
            newCounter = idCounter + 1
        liftIO $ writeIORef dbRef (newIntMap, newCounter)
        testDbDSLInServant dbRef . nextStep $ intToSqlKey idCounter

    go (DelDb key) nextStep = do
        (intMap, counter) <- liftIO $ readIORef dbRef
        let newIntMap = IntMap.delete (sqlKeyToInt key) intMap
        liftIO $ writeIORef dbRef (newIntMap, counter)
        testDbDSLInServant dbRef $ nextStep ()

    go (UpdateDb key blogPost) nextStep = do
        (intMap, counter) <- liftIO $ readIORef dbRef
        let newIntMap = IntMap.insert (sqlKeyToInt key) blogPost intMap
        liftIO $ writeIORef dbRef (newIntMap, counter)
        testDbDSLInServant dbRef $ nextStep ()

    go (ThrowDb servantErr) _ =
        throwError servantErr

    sqlKeyToInt :: Key BlogPost -> Int
    sqlKeyToInt key = fromInteger . toInteger $ fromSqlKey key

    intToSqlKey :: Int -> Key BlogPost
    intToSqlKey int = toSqlKey . fromInteger $ toInteger int

app :: IO Application
app = do
    dbRef <- newIORef (IntMap.empty, 1)
    return $ serve blogPostApiProxy $ server $ testDbDSLInServant dbRef

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
    postJson :: (ToJSON a) => ByteString -> a -> WaiSession SResponse
    postJson path =
        request methodPost path [("Content-Type", "application/json")] . encode

    putJson :: (ToJSON a) => ByteString -> a -> WaiSession SResponse
    putJson path =
        request methodPut path [("Content-Type", "application/json")] . encode

    shouldRespondWithJson :: (ToJSON a)
                          => WaiSession SResponse
                          -> (Integer, a)
                          -> WaiExpectation
    shouldRespondWithJson req (expectedStatus, expectedValue) =
        let matcher = (fromInteger expectedStatus)
                        { matchBody = Just $ encode expectedValue }
        in shouldRespondWith req matcher

    testBlogPost :: BlogPost
    testBlogPost = BlogPost "title" "content"

main :: IO ()
main = hspec spec

