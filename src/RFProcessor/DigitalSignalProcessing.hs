{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module RFProcessor.DigitalSignalProcessing
    ( frequencyModulate
    , frequencyDemodulate
    , waveletTransform
    , adaptiveFilter
    , SignalMetadata(..)
    , runRFProcessor
    ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Complex
import Utils.Wavelet (Wavelet(..), getWaveletFilterPair, WaveletError(..))
import Numeric.LinearAlgebra (Vector, fromList, toList, (!), (<>))
import qualified Numeric.LinearAlgebra as LA

-- | Signal metadata containing sampling rate, center frequency, and gain.
data SignalMetadata = SignalMetadata
    { sampleRate      :: Double
    , centerFrequency :: Double
    , gain            :: Double
    } deriving (Show)

-- | RFProcessorT monad for managing signal processing
newtype RFProcessorT m a = RFProcessorT { runRFProcessorT :: ReaderT Signal (StateT SignalMetadata m) a }
    deriving (Functor, Applicative, Monad, MonadReader Signal, MonadState SignalMetadata, MonadIO)

-- | Run the RFProcessor monad
runRFProcessor :: Monad m => RFProcessorT m a -> Signal -> SignalMetadata -> m (a, SignalMetadata)
runRFProcessor (RFProcessorT action) signal metadata =
    runStateT (runReaderT action signal) metadata

-- | Frequency modulation of the signal
frequencyModulate :: Double                  -- ^ Carrier frequency in Hz
                  -> RFProcessorT IO Signal  -- ^ Modulated signal
frequencyModulate carrierFreq = do
    Signal _ samples <- ask
    metadata <- get
    let samplePeriod = 1 / sampleRate metadata
        times = [0..fromIntegral (LA.size samples - 1)] / samplePeriod
        modulatedSignal = LA.fromList $ zipWith (\s t -> s * cis (2 * pi * carrierFreq * t)) (LA.toList samples) times
    return $ Signal (sampleRate metadata) (LA.vector modulatedSignal)

-- | Frequency demodulation of the signal
frequencyDemodulate :: Double                  -- ^ Carrier frequency in Hz
                    -> RFProcessorT IO Signal  -- ^ Demodulated signal
frequencyDemodulate carrierFreq = do
    Signal _ samples <- ask
    metadata <- get
    let samplePeriod = 1 / sampleRate metadata
        times = [0..fromIntegral (LA.size samples - 1)] / samplePeriod
        demodulatedSignal = LA.fromList $ zipWith (\s t -> s * cis (-2 * pi * carrierFreq * t)) (LA.toList samples) times
    return $ Signal (sampleRate metadata) (LA.vector demodulatedSignal)

-- | Perform wavelet transform using specified wavelet and level
waveletTransform :: Wavelet
                 -> Int
                 -> RFProcessorT IO (Either WaveletError ([Double], [Double]))
waveletTransform wavelet level = do
    Signal _ samples <- ask
    let realSignal = map realPart $ LA.toList samples
        imagSignal = map imagPart $ LA.toList samples
    case getWaveletFilterPair wavelet of
        Left err -> return $ Left err
        Right (lowPass, highPass) -> do
            let (transformedReal, detailReal) = multiLevelDWT realSignal level lowPass highPass
                (transformedImag, detailImag) = multiLevelDWT imagSignal level lowPass highPass
                transformed = zipWith (+) transformedReal transformedImag
                detailCoeffs = zipWith (+) detailReal detailImag
            return $ Right (transformed, detailCoeffs)

-- | Multi-level DWT implementation
multiLevelDWT :: [Double]               -- ^ Input signal
              -> Int                    -- ^ Decomposition level
              -> [Double]               -- ^ Low-pass filter coefficients
              -> [Double]               -- ^ High-pass filter coefficients
              -> ([Double], [Double])   -- ^ (Approximation, Details)
multiLevelDWT signal level lowPass highPass = go signal level []
  where
    go s 0 detailCoeffs = (s, concat (reverse detailCoeffs))
    go s l detailCoeffs =
        let (a, d) = dwt s lowPass highPass
        in go a (l - 1) (d : detailCoeffs)

-- | Single level DWT using specified wavelet filters
dwt :: [Double]         -- ^ Input signal
    -> [Double]         -- ^ Low-pass filter coefficients
    -> [Double]         -- ^ High-pass filter coefficients
    -> ([Double], [Double]) -- ^ (Approximation, Detail)
dwt signal lowPassFilter highPassFilter = (approximation, detail)
  where
    approximation = downsample $ convolve signal lowPassFilter
    detail        = downsample $ convolve signal highPassFilter

-- | Convolution operation
convolve :: [Double] -> [Double] -> [Double]
convolve xs hs = [ sum $ zipWith (*) (take (length hs) (drop n xs ++ repeat 0)) hsRev | n <- [0..length xs - 1] ]
  where
    hsRev = reverse hs

-- | Downsample by taking every other sample
downsample :: [Double] -> [Double]
downsample [] = []
downsample (x:xs) = x : case xs of
    (_:rest) -> downsample rest
    []       -> []

-- | Adaptive filter implementation using Least Mean Squares (LMS) algorithm
adaptiveFilter :: Signal     -- ^ Desired signal
               -> Double     -- ^ Learning rate (mu)
               -> Int        -- ^ Filter order
               -> RFProcessorT IO Signal
adaptiveFilter desired mu order = do
    Signal _ input <- ask
    let desiredList = LA.toList $ LA.vector $ LA.toList desired
        inputList = LA.toList input
        -- Initialize filter coefficients to zero
        lmsCoeffs = replicate order 0.0
        -- Perform LMS adaptive filtering
        (filtered, finalCoeffs) = lms adaptiveFilterStep inputList desiredList lmsCoeffs []
    return $ Signal 0 (LA.fromList filtered)
  where
    -- LMS filter step
    adaptiveFilterStep :: [Double] -> Double -> [Double] -> [Double] -> (Double, [Double])
    adaptiveFilterStep history d coeffs =
        let y = sum $ zipWith (*) coeffs (reverse history)
            e = d - y
            coeffs' = zipWith (\c h -> c + mu * e * h) coeffs (reverse history)
        in (y, coeffs')

    -- LMS algorithm
    lms :: ( [Double] -> Double -> [Double] -> [Double] -> (Double, [Double]) )
        -> [Double] -> [Double] -> [Double] -> [Double] -> ([Double], [Double])
    lms _ [] _ _ acc = (reverse acc, [])
    lms step (x:xs) (d:ds) coeffs acc =
        let history = take (length coeffs) (x : acc)
            (y, coeffs') = step history d coeffs
        in lms step xs ds coeffs' (y : acc)
    lms _ _ _ _ acc = (reverse acc, [])
