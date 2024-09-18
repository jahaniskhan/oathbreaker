{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RFProcessor.DigitalSignalProcessing
    ( Signal
    , SignalMetadata(..)
    , RFProcessorT(..)
    , runRFProcessor
    , applyFilter
    , frequencyModulate
    , frequencyDemodulate
    , waveletTransform
    , adaptiveFilter
    ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Complex
import qualified Data.Vector.Storable as V

-- Signal type (using Complex Double for I/Q data)
type Signal = [Complex Double]

-- Metadata for our signal
data SignalMetadata = SignalMetadata
    { sampleRate      :: Double
    , centerFrequency :: Double
    , gain            :: Double
    } deriving (Show)

-- RFProcessorT monad for managing signal processing
newtype RFProcessorT m a = RFProcessorT { runRFProcessorT :: ReaderT Signal (StateT SignalMetadata m) a }
    deriving (Functor, Applicative, Monad, MonadReader Signal, MonadState SignalMetadata)

-- Run the RFProcessor monad
runRFProcessor :: Monad m => RFProcessorT m a -> Signal -> SignalMetadata -> m (a, SignalMetadata)
runRFProcessor (RFProcessorT action) signal metadata = 
    runStateT (runReaderT action signal) metadata

-- Apply a filter to the signal
applyFilter :: Monad m => (Double -> Double) -> RFProcessorT m Signal
applyFilter filterFunc = do
    signal <- ask
    metadata <- get
    let n = length signal
        freqs = [ fromIntegral i * sampleRate metadata / fromIntegral n | i <- [0 .. n - 1] ]
        filteredSignal = zipWith (*) signal (map (:+ 0) (map filterFunc freqs))
    return filteredSignal

-- Frequency modulation
frequencyModulate :: Monad m => Double -> RFProcessorT m Signal
frequencyModulate carrierFreq = do
    signal <- ask
    metadata <- get
    let sampleRateVal = sampleRate metadata
        t = [ fromIntegral i / sampleRateVal | i <- [0 .. length signal - 1] ]
        modulatedSignal = zipWith (\s ti -> s * exp (0 :+ (2 * pi * carrierFreq * ti))) signal t
    return modulatedSignal

-- Frequency demodulation
frequencyDemodulate :: Monad m => Double -> RFProcessorT m Signal
frequencyDemodulate carrierFreq = do
    signal <- ask
    metadata <- get
    let sampleRateVal = sampleRate metadata
        t = [ fromIntegral i / sampleRateVal | i <- [0 .. length signal - 1] ]
        demodulatedSignal = zipWith (\s ti -> s * exp (0 :+ (-2 * pi * carrierFreq * ti))) signal t
    return demodulatedSignal

-- | Perform wavelet transform
waveletTransform :: Int -> [Complex Double] -> [Complex Double]
waveletTransform level xs
    | level <= 0 = xs
    | otherwise =
        let (evensList, oddsList) = unzip [(x, y) | ((i, x), (_, y)) <- zip (zip [(0::Int)..] xs) (zip [(1::Int)..] (drop 1 xs)), even i]
            paired = zip evensList oddsList
            averages = map (\(a, b) -> (a + b) / sqrt 2) paired
            differences = map (\(a, b) -> (a - b) / sqrt 2) paired
        in averages ++ differences

-- | Adaptive filtering using LMS algorithm
adaptiveFilter :: Int -> Double -> V.Vector (Complex Double) -> V.Vector (Complex Double)
adaptiveFilter filterLength stepSize signal =
    V.unfoldr go (V.replicate filterLength (0 :+ 0), signal)
  where
    go (weights, xs)
      | V.null xs = Nothing
      | otherwise =
          let x = V.head xs
              xVec = V.take filterLength (V.cons x xs)
              (y, newWeights) = lms x xVec weights stepSize
          in Just (y, (newWeights, V.tail xs))

lms :: Complex Double -> V.Vector (Complex Double) -> V.Vector (Complex Double) -> Double -> (Complex Double, V.Vector (Complex Double))
lms x xVec weights stepSize =
    let y = V.sum $ V.zipWith (*) weights xVec
        e = x - y
        newWeights = V.zipWith (\w xi -> w + (stepSize :+ 0) * e * conjugate xi) weights xVec
    in (y, newWeights)

-- Commented out unused functions
{-
-- | Simple Discrete Wavelet Transform (Haar wavelet)
dwt :: [Double] -> [Double]
dwt [] = []
dwt [x] = [x]
dwt xs = 
    let pairs = zip (evens xs) (odds xs)
        averages = map (\(a, b) -> (a + b) / sqrt 2) pairs
        differences = map (\(a, b) -> (a - b) / sqrt 2) pairs
    in averages ++ differences
  where
    evens ys = map fst . filter (even . snd) $ zip ys [0 :: Int ..]
    odds ys = map fst . filter (odd . snd) $ zip ys [0 :: Int ..]

-- Add this simple wavelet transform function
simpleWaveletTransform :: [Complex Double] -> [Complex Double]
simpleWaveletTransform [] = []
simpleWaveletTransform [x] = [x]
simpleWaveletTransform xs =
    let (evens, odds) = unzip $ zip (everyOther xs) (everyOther $ drop 1 xs)
        averages = zipWith (\a b -> (a + b) / sqrt 2) evens odds
        differences = zipWith (\a b -> (a - b) / sqrt 2) evens odds
    in averages ++ differences

everyOther :: [a] -> [a]
everyOther [] = []
everyOther (x:_:xs) = x : everyOther xs
everyOther [x] = [x]
-}
