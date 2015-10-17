{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.Catch (catch, throwM)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Operational (ProgramViewT(..), singleton, view)
import Control.Monad.Trans.Either (EitherT)
import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import Data.IntMap.Lazy (IntMap)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Lazy as IntMap
import Database.Persist (Key)
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Network.HTTP.Types.Method (methodPost)
import Network.Wai (Application)
import Network.Wai.Test (SResponse)
import Servant.Server (ServantErr(..), Server, serve)
import System.IO
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Lib

-- testDbDSL :: DbDSL a -> IO a
-- testDbDSL dbDSL =
--     hPutStrLn stderr "in testDbDSLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL"
--     case view dbDSL of
--         Return a -> a
--         -- a :>>= nextStep -> go a nextStep
--         a :>>= nextStep -> undefined
--   where
    -- go :: DbAction a -> (a -> DbDSL b) -> SqlPersistT (EitherT ServantErr IO) b
    -- go (GetDb key) nextStep = do
    --     maybeVal <- get key
    --     runDbDSLInPersistent $ nextStep maybeVal
    -- go (InsertDb val) nextStep = do
    --     key <- insert val
    --     runDbDSLInPersistent $ nextStep key
    -- go (DelDb key) nextStep = do
    --     delete key
    --     runDbDSLInPersistent $ nextStep ()
    -- go (GetByDb uniqueVal) nextStep = do
    --     maybeEntityVal <- getBy uniqueVal
    --     runDbDSLInPersistent $ nextStep maybeEntityVal
    -- go (UpdateDb key val) nextStep = do
    --     replace key val
    --     runDbDSLInPersistent $ nextStep ()
    -- go (ThrowDb servantErr@(ServantErr httpStatusCode httpStatusString _ _)) _ = do
    --     -- In actual usage, you may need to rollback the database
    --     -- connection here.  It doesn't matter for this simple
    --     -- demonstration, but in production you'll probably want to roll
    --     -- back the current transaction when you use 'Throw'.
    --     -- conn <- ask
    --     -- liftIO $ connRollback conn (getStmtConn conn)
    --     liftIO $ putStrLn $
    --         "error occured: " ++ show (httpStatusCode,httpStatusString)
    --     throwM servantErr

-- testDbDSLInServant :: DbDSL a
--                    -> EitherT ServantErr IO a
-- testDbDSLInServant dbDSL =
--     runSqlConn (runDbDSLInPersistent dbDSL) conn
--         `catch` \(err::ServantErr) -> throwError err
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
        liftIO $ putStrLn $ "In GetDB, intMap is " ++ show intMap
        let maybeBlogPost = IntMap.lookup (sqlKeyToInt key) intMap
        testDbDSLInServant dbRef $ nextStep maybeBlogPost

    go (InsertDb blogPost) nextStep = do
        (intMap, idCounter) <- liftIO $ readIORef dbRef
        liftIO $ putStrLn $ "In InsertDb beginning, intMap is " ++ show intMap
        let newIntMap = IntMap.insert idCounter blogPost intMap
            newCounter = idCounter + 1
        liftIO $ writeIORef dbRef (newIntMap, newCounter)
        testDbDSLInServant dbRef . nextStep $ intToSqlKey idCounter

    go (DelDb key) nextStep = do
        -- delete key
        -- runDbDSLInPersistent $ nextStep ()
        undefined

    go (UpdateDb key val) nextStep = do
        -- replace key val
        -- runDbDSLInPersistent $ nextStep ()
        undefined

    go (ThrowDb servantErr@(ServantErr httpStatusCode httpStatusString _ _)) _ = do
        liftIO $ putStrLn $
            "error occured: " ++ show (httpStatusCode,httpStatusString)
        throwError servantErr

    sqlKeyToInt :: Key BlogPost -> Int
    sqlKeyToInt key = fromInteger . toInteger $ fromSqlKey key

    intToSqlKey :: Int -> Key BlogPost
    intToSqlKey int = toSqlKey . fromInteger $ toInteger int

app :: IO Application
app = do
    dbRef <- newIORef (IntMap.empty, 1)
    return $ serve myApiType $ server $ testDbDSLInServant dbRef

spec :: Spec
spec = with app $ do
    describe "GET /blogpost/read/1" $ do
        it "responds with 404 because nothing has been inserted" $ do
            liftIO $ putStrLn "first test..."
            get "/blogpost/read/1" `shouldRespondWith` 404

        it "responds with 200 after inserting something" $ do
            liftIO $ putStrLn "second test..."
            postJson "/blogpost/create" (BlogPost "title" "content")
                `shouldRespondWith` 201
            get "/blogpost/read/1" `shouldRespondWith` 200

        -- it "responds with 200 / 'hello'" $ do
        --     get "/" `shouldRespondWith` "hello" {matchStatus = 200}

        -- it "has 'Content-Type: text/plain; charset=utf-8'" $ do
        --     get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

  -- describe "GET /some-json" $ do
    -- it "responds with some JSON" $ do
      -- get "/some-json" `shouldRespondWith` [json|{foo: 23, bar: 42}|]

postJson :: (ToJSON a) => ByteString -> a -> WaiSession SResponse
postJson path =
    request methodPost path [("Content-Type", "application/json")] . encode

main :: IO ()
main = hspec spec

