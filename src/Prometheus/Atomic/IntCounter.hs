{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples     #-}
module Prometheus.Atomic.IntCounter
  ( IntCounter(..)
  , intCounter
  , newIntCounter
  , readIntCounter
  , incIntCounter
  , addIntCounter
  ) where

import           Control.Monad.Primitive
import qualified Data.ByteString.Char8    as BSC
import           Data.Primitive.PrimArray
import           GHC.Exts                 (Int (..), atomicReadIntArray#,
                                           fetchAddIntArray#)
import           Prometheus

newtype IntCounter = IntCounter
  { unIntCounter :: MutablePrimArray (PrimState IO) Int
  }


incIntCounter :: IntCounter -> IO ()
incIntCounter = addIntCounter 1

addIntCounter :: Int -> IntCounter -> IO ()
addIntCounter !(I# i) !(IntCounter (MutablePrimArray arr)) =
  primitive $ \s0 ->
     let !(# s1, _ #) = fetchAddIntArray# arr 0# i s0
     in (# s1, () #)

readIntCounter :: IntCounter -> IO Int
readIntCounter !(IntCounter (MutablePrimArray arr)) =
  primitive $ \s0 ->
     let !(# s1, i #) = atomicReadIntArray# arr 0# s0
     in (# s1, I# i #)

newIntCounter :: IO IntCounter
newIntCounter = do
  arr <- newPrimArray 1
  IntCounter arr <$ setPrimArray arr 0 1 0


intCounter :: Info -> Metric IntCounter
intCounter info = intCounterLabeled info []

intCounterLabeled :: Info -> LabelPairs -> Metric IntCounter
intCounterLabeled info lab = Metric $ do
  c <- newIntCounter
  pure (c, sample c)
  where
    sample c = mkSample <$> readIntCounter c
    mkSample v = [ SampleGroup info CounterType
                    [Sample (metricName info) lab (BSC.pack (show v))]
                 ]

