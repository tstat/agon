module Agon where

import           Prelude

import           Agon.Stats (RequestInfo(..), Report(..), reporting, reportFold, formatReport)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Exception (finally)
import qualified Control.Foldl as L
import           Control.Monad
import           Data.Foldable (traverse_)
import           Data.IORef
import           Data.Time (getCurrentTime)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)
import           Pipes
import           Pipes.Concurrent
import qualified Pipes.Prelude as Pipes
import           System.Console.Regions
  ( ConsoleRegion, RegionLayout(Linear), displayConsoleRegions, withConsoleRegion
  , setConsoleRegion, finishConsoleRegion
  )

bench
  :: IORef Report
  -> Producer Request IO ()
  -> Int
  -> Int
  -> IO Report
bench reportRef prod reqCount threadCount = do
  m <- newManager tlsManagerSettings { managerConnCount = threadCount }
  (reqOutput, reqInput, reqSeal) <- spawn' $ bounded (threadCount * 10)
  (statOutput, statInput, statSeal) <- spawn' unbounded
  η <- async . runEffect $ prod >-> Pipes.take reqCount >-> toOutput reqOutput
  γ <- replicateM threadCount . async . runEffect $
    fromInput reqInput >-> worker m >-> toOutput statOutput
  ψ <- async $ L.impurely Pipes.foldM (reporting reportRef reportFold) (fromInput statInput)
  wait η
  atomically reqSeal
  traverse_ wait γ
  atomically statSeal
  wait ψ

worker :: Manager -> Pipe Request RequestInfo IO ()
worker m = do
  req <- await
  yield =<< liftIO (do
    st <- getCurrentTime
    res <- httpLbs req m
    et <- getCurrentTime
    pure $ RequestInfo st et (statusCode $ responseStatus res))
  worker m

benchWithStreamingStats
  :: Int
  -> Int
  -> Producer Request IO ()
  -> IO ()
benchWithStreamingStats reqCount threadCount prod = do
  ref <- newIORef (Report Nothing 0 0 Nothing Nothing 0 0 0 0) :: IO (IORef Report)
  α <- async $ bench ref prod reqCount threadCount
  β <- async $ displayConsoleRegions $ do
    displayCount ref
  _ <- wait α
  cancel β
  where
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
