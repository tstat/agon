module Agon.Stats
  ( Report(..)
  , RequestInfo(..)
  , reportFold
  , stepFold
  , getFold
  , formatReport
  ) where

import           Prelude

import qualified Control.Foldl as L
import           Data.Coerce (coerce)
import           Data.Profunctor (lmap)
import           Data.Semigroup (Min(..), Max(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, NominalDiffTime, diffUTCTime)
import           Fmt (Builder, fmt, padBothF, fixedF, maybeF, commaizeF, exptF)

reportFold :: L.Fold RequestInfo Report
reportFold = Report
  <$> reportRPS
  <*> fromDiffTime L.mean
  <*> fromDiffTime L.variance
  <*> fromDiffTime L.minimum
  <*> fromDiffTime L.maximum
  <*> reqStatusCounter (\x -> div x 100 == 2)
  <*> reqStatusCounter (\x -> div x 100 == 3)
  <*> reqStatusCounter (\x -> div x 100 == 4)
  <*> reqStatusCounter (\x -> div x 100 == 5)
  where
    reportRPS :: L.Fold RequestInfo (Maybe Double)
    reportRPS = L.Fold step start done
      where
        step
          :: (Maybe (Min UTCTime), Maybe (Max UTCTime), Int)
          -> RequestInfo
          -> (Maybe (Min UTCTime), Maybe (Max UTCTime), Int)
        step (minStart, maxEnd, reqCount) (RequestInfo { _riStartTime = st, _riEndTime = et }) =
          (minStart <> Just (Min st), maxEnd <> Just (Max et), succ reqCount)

        start = (Nothing, Nothing, 0)

        done (st, et, reqCount) = fmap realToFrac
          $ (fromIntegral reqCount /) <$> (diffUTCTime <$> coerce et <*> coerce st)

    fromDiffTime :: ∀ b. L.Fold NominalDiffTime b -> L.Fold RequestInfo b
    fromDiffTime = lmap (\(RequestInfo { _riStartTime = st, _riEndTime = et }) -> diffUTCTime et st)

    reqStatusCounter :: (Int -> Bool) -> L.Fold RequestInfo Int
    reqStatusCounter predicate = L.Fold step 0 id
      where
        step b a = if predicate (_riStatusCode a) then succ b else b


data RequestInfo
  = RequestInfo
  { _riStartTime :: UTCTime
  , _riEndTime :: UTCTime
  , _riStatusCode :: Int
  } deriving Show

data Report
  = Report
  { rps :: Maybe Double
  , latencyMean :: NominalDiffTime
  , latencyVariance :: NominalDiffTime
  , latencyMin :: Maybe NominalDiffTime
  , latencyMax :: Maybe NominalDiffTime
  , _2xx :: Int
  , _3xx :: Int
  , _4xx :: Int
  , _5xx :: Int
  }
  deriving Show

stepFold :: L.Fold a b -> a -> (b, L.Fold a b)
stepFold (L.Fold step start done) a =
  let st = step start a
      b = done st
  in (b, L.Fold step st done)

getFold :: L.Fold a b -> b
getFold (L.Fold _ start done) = done start

formatReport :: Report -> Text
formatReport Report{..} = fmt $ mconcat
  [ mkTable
    [ "requests / second"
    , "mean"
    , "variance"
    , "min"
    , "max"
    ]
    [ maybeF (fixedF 5 <$> rps)
    , exptF 4 latencyMean
    , exptF 4 latencyVariance
    , maybeF (exptF 4 <$> latencyMin)
    , maybeF (exptF 4 <$> latencyMax)
    ]
  , "\n\n"
  , mkTable
    [ "2xx"
    , "3xx"
    , "4xx"
    , "5xx"
    ]
    [ commaizeF _2xx
    , commaizeF _3xx
    , commaizeF _4xx
    , commaizeF _5xx
    ]
  ]
  where
    mkTable :: [Text] -> [Builder] -> Builder
    mkTable hdrs dat = mconcat
      [ mconcat $ map (\(pad,hdr) -> padBothF pad ' ' hdr) (zip padList hdrs)
      , "\n"
      , mconcat $ map (\(pad, x) -> padBothF pad ' ' x) (zip padList dat)
      ]
      where
        padList :: [Int]
        padList = map (\x -> max (T.length x + 2) 16) hdrs
