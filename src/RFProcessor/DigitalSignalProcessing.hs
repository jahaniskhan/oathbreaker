{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module RFProcessor.DigitalSignalProcessing
    ( Signal
    , SignalMetadata(..)
    , RFProcessorT(..)
    , runRFProcessor
    , frequencyModulate
    , frequencyDemodulate
    , waveletTransform
    , adaptiveFilter
    ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Complex
import Utils.Wavelet (Wavelet(..), getWaveletFilterPair, WaveletError(..))

-- | Signal type (using Complex Double for I/Q data)
type Signal = [Complex Double]

-- | Metadata for the signal
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
    signal <- ask
    metadata <- get
    let samplePeriod = 1 / sampleRate metadata
        times = [n * samplePeriod | n <- [0..fromIntegral (length signal - 1)]]
        modulatedSignal = zipWith (\s t -> s * cis (2 * pi * carrierFreq * t)) signal times
    return modulatedSignal

-- | Frequency demodulation of the signal
frequencyDemodulate :: Double                  -- ^ Carrier frequency in Hz
                    -> RFProcessorT IO Signal  -- ^ Demodulated signal
frequencyDemodulate carrierFreq = do
    signal <- ask
    metadata <- get
    let samplePeriod = 1 / sampleRate metadata
        times = [n * samplePeriod | n <- [0..fromIntegral (length signal - 1)]]
        demodulatedSignal = zipWith (\s t -> s * cis (-2 * pi * carrierFreq * t)) signal times
    return demodulatedSignal

-- | Perform wavelet transform using specified wavelet and level
waveletTransform :: Wavelet
                 -> Int
                 -> RFProcessorT IO (Either WaveletError ([Double], [Double]))
waveletTransform wavelet level = do
    signal <- ask
    let realSignal = map realPart signal
        imagSignal = map imagPart signal
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
    signal <- ask
    let lms :: [Double] -> [Complex Double] -> [Complex Double] -> [Complex Double] -> [Complex Double] -> [Complex Double]
        lms coeffs [] [] _ output = output
        lms coeffs (x:xs) [] history output =
            let y = sum $ zipWith (*) (map (:+ 0) coeffs) history
                coeffs' = zipWith (+) coeffs (map ((* (mu * realPart y))) (map realPart history))
            in lms coeffs' xs [] (x : take (order - 1) (history ++ repeat (0 :+ 0))) (y : output)
        lms coeffs [] (d:ds) history output = 
            let y = sum $ zipWith (*) (map (:+ 0) coeffs) history
                e = d - y
                coeffs' = zipWith (+) coeffs (map ((* (mu * realPart e))) (map realPart history))
            in lms coeffs' [] ds ((0 :+ 0) : take (order - 1) (history ++ repeat (0 :+ 0))) (y : output)
        lms coeffs (x:xs) (d:ds) history output =
            let y = sum $ zipWith (*) (map (:+ 0) coeffs) history
                e = d - y
                coeffs' = zipWith (+) coeffs (map ((* (mu * realPart e))) (map realPart history))
            in lms coeffs' xs ds (x : take (order - 1) (history ++ repeat (0 :+ 0))) (y : output)
        paddedInput = replicate order (0 :+ 0) ++ signal
        desiredPadded = replicate order (0 :+ 0) ++ desired
        coeffsInit = replicate order 0
        historyInit = replicate order (0 :+ 0)
    return $ reverse $ lms coeffsInit paddedInput desiredPadded historyInit []
