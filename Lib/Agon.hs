module Agon where

import           Prelude

import           Agon.Stats (RequestInfo(..), Report(..), stepFold, formatReport, reportFold, getFold)
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import qualified Control.Foldl as L
import qualified Control.Immortal as Immortal
import           Control.Monad
import           Data.Foldable
import           Data.Function
import           Data.IORef
import qualified Data.Text as T
import           Data.Time (getCurrentTime)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)
import           System.Console.Regions
  ( ConsoleRegion, RegionLayout(Linear), displayConsoleRegions, withConsoleRegion
  , setConsoleRegion, finishConsoleRegion
  )

bench
  :: IORef Report
  -> TQueue HttpException
  -> [Request]
  -> Int
  -> Int
  -> IO Report
bench reportRef exQueue reqStream requestCount threadCount = do
  m <- newManager tlsManagerSettings { managerConnCount = threadCount }
  reqQueue :: TBQueue Request <- atomically $ newTBQueue (10 * fromIntegral threadCount)
  statQueue :: TQueue RequestInfo <- atomically newTQueue
  workers <- replicateM threadCount $ Immortal.create \_ -> fix \f -> do
    req <- atomically $ readTBQueue reqQueue
    st <- getCurrentTime
    res <- httpLbs req m `catch` \(e :: HttpException) -> do
      atomically $ writeTQueue exQueue e
      throwIO e
    et <- getCurrentTime
    atomically $ writeTQueue statQueue $ RequestInfo st et (statusCode $ responseStatus res)
    f
  statCollector <- async $ collectStats reportRef requestCount statQueue
  putter <- async $ (fix \f rs -> do
                       atomically $ writeTBQueue reqQueue (head rs)
                       f (tail rs)) reqStream
  report <- wait statCollector
  traverse_ Immortal.stop workers
  cancel putter
  pure report

collectStats :: IORef Report -> Int -> TQueue RequestInfo -> IO Report
collectStats reportRef reqCount riQueue = go reportFold reqCount
  where
    go :: L.Fold RequestInfo Report -> Int -> IO Report
    go φ n
      | n <= 0 = pure (getFold φ)
      | otherwise = do
          requestInfo <- atomically $ readTQueue riQueue
          let (report, φ') = stepFold φ requestInfo
          writeIORef reportRef report
          go φ' (pred n)

benchWithStreamingStats
  :: Int
  -> Int
  -> [Request]
  -> IO ()
benchWithStreamingStats reqCount threadCount prod = displayConsoleRegions do
  ref :: IORef Report <- newIORef (Report Nothing 0 0 Nothing Nothing 0 0 0 0)
  exQueue :: TQueue HttpException <- atomically newTQueue
  α <- async $ bench ref exQueue prod reqCount threadCount
  countThread <- async $ displayCount ref
  errThread <- async $ displayErrors exQueue
  _ <- wait α
  cancel countThread
  cancel errThread
  where
    displayErrors :: TQueue HttpException -> IO ()
    displayErrors exQueue = withConsoleRegion Linear \cr -> 1 & fix \φ (errCnt :: Int) -> do
      ex <- atomically $ readTQueue exQueue
      setConsoleRegion cr $ "HTTP client errors: " <> T.pack (show errCnt)
      withConsoleRegion Linear \cr' -> finishConsoleRegion cr' (show ex)
      φ (succ errCnt)

    displayCount :: IORef Report -> IO ()
    displayCount ref = withConsoleRegion Linear (\r -> go r `finally` finalGo r)
      where
        go :: ConsoleRegion -> IO ()
        go r = do
          n <- readIORef ref
          setConsoleRegion r $ formatReport n
          threadDelay 150000
          go r

        finalGo :: ConsoleRegion -> IO ()
        finalGo r = do
           n <- readIORef ref
           finishConsoleRegion r $ formatReport n

data ErrorSummary
  = ErrorSummary Int (Maybe SomeException)
