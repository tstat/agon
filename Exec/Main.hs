module Main ( main ) where

import           Prelude

import           Agon (benchWithStreamingStats)
import           Agon.Request (Request, parseRequests, toHttpClientRequest)
import           Control.Monad ( (<=<) )
import           Data.List (intercalate)
import qualified Data.Text as T
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           Text.Read (readEither)

data RunOptions
  = RunOptions
  { _roRequestCount :: Int
  , _roThreadCount  :: Int
  , _roRequests     :: [Request]
  }
  | Help

requestCount :: ∀ f. Applicative f => (Int -> f Int) -> RunOptions -> f RunOptions
requestCount f (RunOptions x a b) = (\x' -> RunOptions x' a b) <$> f x
requestCount _ Help = pure Help

threadCount :: ∀ f. Applicative f => (Int -> f Int) -> RunOptions -> f RunOptions
threadCount f (RunOptions a x b) = (\x' -> RunOptions a x' b) <$> f x
threadCount _ Help = pure Help

main :: IO ()
main = do
  (popts, extra, errs) <- getOpt RequireOrder opts <$> getArgs
  let exitF err = putStr (usageInfo err opts) >> exitFailure
  if null errs then pure () else exitF (intercalate ", " errs)
  case foldr (<=<) pure popts (RunOptions 0 0 []) of
    Left err -> exitF err
    Right Help -> putStr (usageInfo "" opts)
    Right (RunOptions n t _) -> do
      rs' <- maybe (exitF "Failed to parse requests") pure $ parseRequests $ T.pack $ unwords extra
      traverse toHttpClientRequest rs' >>= \case
        [] -> exitF "Must provide at least one request"
        xs -> benchWithStreamingStats n t (cycle xs)
  where
    opts :: [OptDescr (RunOptions -> Either String RunOptions)]
    opts =
      [ Option ['n'] ["request-count"] (ReqArg setRequestCount "") "The number of requests to perform"
      , Option ['t'] ["thread-count"] (ReqArg setThreadCount "") "The number of threads performing requests"
      , Option ['h'] ["help"] (NoArg setHelp) "Display this usage info"
      ]

    setRequestCount :: String -> RunOptions -> Either String RunOptions
    setRequestCount str = requestCount (const $ readEither str)

    setThreadCount :: String -> RunOptions -> Either String RunOptions
    setThreadCount str = threadCount (const $ readEither str)

    setHelp :: RunOptions -> Either String RunOptions
    setHelp _ = Right Help
