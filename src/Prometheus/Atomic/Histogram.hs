{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE UnboxedTuples      #-}
module Prometheus.Atomic.Histogram
  ( Histogram(..)
  , createHistogram
  , observeHistogram
  , sampleHistogram
  , histogram
  , reportHistogram
  ) where

import           Control.Monad.Primitive
import qualified Data.ByteString.Char8    as BSC
import           Data.Function            (fix)
import           Data.IORef               (IORef, atomicModifyIORef', newIORef,
                                           readIORef)
import           Data.Maybe               (fromMaybe)
import           Data.Primitive.ByteArray
import           Data.Primitive.Types
import qualified Data.Text                as Text
import qualified Data.Vector.Primitive    as Vector
import           GHC.Exts                 (Int (..), atomicReadIntArray#,
                                           fetchAddIntArray#)
import           Numeric                  (showFFloat)

import           Prometheus               (Info (..), Metric (..),
                                           Observer (..), Sample (..),
                                           SampleGroup (..), SampleType (..),
                                           doIO)

data Histogram = Histogram
  { hsBuckets :: Vector.Vector Double
  , hsCounts  :: MutableByteArray (PrimState IO)
  , hsTotal   :: IORef Double
  }

data HistogramSample = HistogramSample
  { sampHistogram :: [Int]
  , sampTotal     :: Double
  , sampCount     :: Int
  } deriving stock (Show)

createHistogram :: [Double] -> IO Histogram
createHistogram buckets = do
  vals <- newByteArray size
  fillByteArray vals 0 size 0
  ref <- newIORef 0
  pure $ Histogram (Vector.fromList buckets) vals ref
  where
    len = length buckets + 1
    size = I# (sizeOf# (0 :: Int)) * len

observeHistogram :: Histogram -> Double -> IO ()
observeHistogram Histogram{..} val = do
  primitive $ \s0 ->
    let !(# s1, _ #) = fetchAddIntArray# arr loc 1# s0
    in (# s1, () #)
  atomicModifyIORef' hsTotal $ \old -> (old + val, ())
  where
    !(MutableByteArray arr) = hsCounts
    !(I# loc) = fromMaybe (Vector.length hsBuckets)  $ Vector.findIndex (> val) hsBuckets

readIntArray :: Int -> MutableByteArray (PrimState IO) -> IO Int
readIntArray !(I# i) !(MutableByteArray arr) =
  primitive $ \s0 ->
    case atomicReadIntArray# arr i s0 of
      (# s1, v #) -> (# s1, I# v #)

sampleHistogram :: Histogram -> IO HistogramSample
sampleHistogram Histogram{..} = do
  vals <- reverse <$> fix go 0 []
  tot <- readIORef hsTotal
  -- Note: race condition between total and vals
  let
    cumulative = scanr1 (+) vals
    count = sum vals
  pure $ HistogramSample cumulative tot count
  where
    go next i acc
      | i > len = pure acc
      | otherwise = do
          c <- readIntArray i hsCounts
          next (succ i) (c:acc)

    !len = Vector.length hsBuckets

bShow :: Show a => a -> BSC.ByteString
bShow = BSC.pack . show

histogram :: [Double] -> Info -> Metric Histogram
histogram buckets info = Metric $
  (\hs -> (hs, reportHistogram info hs)) <$> createHistogram buckets

reportHistogram :: Info -> Histogram -> IO [SampleGroup]
reportHistogram info@Info{..} hs@Histogram{..} = do
  samp <- sampleHistogram hs
  pure [SampleGroup info HistogramType $ makeSamples samp]
  where
    countSample HistogramSample{..} =
      Sample (metricName <> "_count") [] $ bShow  sampCount
    sumSample HistogramSample{..} =
      Sample (metricName <> "_sum") [] $ bShow sampTotal
    bucketName = metricName <> "_bucket"
    mkBucket l = Sample bucketName [("le", l)] . bShow
    makeSamples samp@HistogramSample{..} =
      let labels = fmap formatFloat (Vector.toList hsBuckets) <> ["Inf"]
          bucks = uncurry mkBucket  <$> zip labels sampHistogram
      in countSample samp : sumSample samp : bucks

    formatFloat x = Text.pack (showFFloat Nothing x "")

instance Observer Histogram where
  observe hs v = doIO $ observeHistogram hs v
