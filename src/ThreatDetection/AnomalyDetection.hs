{-# LANGUAGE OverloadedStrings #-}

module ThreatDetection.AnomalyDetection
    ( detectAnomalies
    , Anomaly(..)
    , AnomalyType(..)
    , detectSpectrumAnomaliesPCA
    ) where

import Data.Complex (Complex(..), magnitude)
import RFProcessor.SpectralAnalysis (calculatePowerSpectralDensity, detectPeaks)
import Numeric.LinearAlgebra (Matrix, Vector, fromList, toList, fromRows, (!))
import FFT (dft)
import qualified Numeric.LinearAlgebra as LA
import Control.Monad (replicateM)
import Data.Function (on)
import Data.List (minimumBy, sortBy)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

import Utils.MathFunctions (median, medianAbsoluteDeviation)
import Utils.PCA (performPCA, PrincipalComponents(..))

-- | Represents an anomaly in the signal
data Anomaly = Anomaly
    { anomalyFrequencyBin :: Int
    , anomalyMagnitude    :: Double
    , anomalyType         :: AnomalyType
    , anomalyScore        :: Double  -- New field for anomaly score
    } deriving (Show, Eq)

-- | Types of anomalies
data AnomalyType = UnexpectedPeak 
                 | SignalDropout 
                 | NoiseBurst 
                 | SpectrumAnomaly 
                 | TimeFrequencyAnomaly
    deriving (Show, Eq)

-- | Main function to detect anomalies using various techniques
detectAnomalies :: [Complex Double] -- ^ Input signal
                -> Double           -- ^ Base threshold
                -> Int              -- ^ Score threshold
                -> Int              -- ^ Window size
                -> [Anomaly]
detectAnomalies signal baseThreshold scoreThreshold windowSize = 
    let psd = calculatePowerSpectralDensity signal
        stftResult = applySTFT signal 128
        weightedThreshold = calculateWeightedThreshold psd baseThreshold
        basicAnomalies = detectBasicAnomalies signal psd weightedThreshold
        stftAnomalies = detectAnomaliesSTFT stftResult weightedThreshold
        pcaAnomalies = detectSpectrumAnomaliesPCA (preparePsdForPCA psd) weightedThreshold
        allAnomalies = basicAnomalies ++ stftAnomalies ++ pcaAnomalies
        scoredAnomalies = scoreAnomalies allAnomalies windowSize
    in filter (\a -> anomalyScore a > fromIntegral scoreThreshold) scoredAnomalies

-- | Calculate weighted threshold based on signal properties
calculateWeightedThreshold :: [Double] -- ^ Power Spectral Density
                           -> Double   -- ^ Base threshold
                           -> Double
calculateWeightedThreshold psd baseThreshold =
    case (median psd, medianAbsoluteDeviation psd) of
        (Just medianPSD, Just madPSD) ->
            baseThreshold * (medianPSD + 2 * madPSD)
        _ -> baseThreshold * 2  -- Fallback in case psd is empty

-- | Apply Short-Time Fourier Transform to the signal.
applySTFT :: [Complex Double] -- ^ Input signal
          -> Int              -- ^ Window size
          -> [[Complex Double]] -- ^ List of FFT results per window
applySTFT signal windowSize =
    let windows = slidingWindows windowSize signal
    in map (V.toList . dft . V.fromList) windows  -- Use 'dft' instead of 'fft'

-- | Detect basic anomalies (peaks, dropouts, bursts)
detectBasicAnomalies :: [Complex Double] -- ^ Input signal
                     -> [Double]        -- ^ Power Spectral Density
                     -> Double          -- ^ Weighted threshold
                     -> [Anomaly]
detectBasicAnomalies signal psd threshold = 
    let peaks = detectPeaks psd 5
        avgMagnitude = if null psd then 0 else sum psd / fromIntegral (length psd)
        localThresholds = calculateLocalThresholds psd
        unexpectedPeaks = [ Anomaly i v UnexpectedPeak 1.0
                          | (i, v) <- peaks
                          , v > (localThresholds !! i) * avgMagnitude
                          ]
        signalDropouts = detectSignalDropouts signal avgMagnitude
        noiseBursts = detectNoiseBursts signal avgMagnitude
    in unexpectedPeaks ++ signalDropouts ++ noiseBursts

-- | Calculate local thresholds for peak detection
calculateLocalThresholds :: [Double] -> [Double]
calculateLocalThresholds psd = map (\v -> v * 1.5) psd  -- Example: Adjust factor as needed

-- | Detect signal dropouts based on average magnitude
detectSignalDropouts :: [Complex Double] -- ^ Input signal
                     -> Double            -- ^ Average magnitude
                     -> [Anomaly]
detectSignalDropouts signal avgMag = 
    [ Anomaly i v SignalDropout 1.0
    | (i, v) <- zip [0..] (map magnitude signal)
    , v < avgMag * 0.5  -- Example threshold
    ]

-- | Detect noise bursts based on average magnitude
detectNoiseBursts :: [Complex Double] -- ^ Input signal
                  -> Double            -- ^ Average magnitude
                  -> [Anomaly]
detectNoiseBursts signal avgMag = 
    [ Anomaly i v NoiseBurst 1.0
    | (i, v) <- zip [0..] (map magnitude signal)
    , v > avgMag * 2.0  -- Example threshold
    ]

-- | Detect anomalies within a single window using STFT analysis
detectAnomaliesInWindow :: Double                       -- ^ Threshold
                         -> (Int, [Complex Double])      -- ^ Window index and data
                         -> [Anomaly]
detectAnomaliesInWindow threshold (windowIndex, windowData) =
    let psd = calculatePowerSpectralDensity windowData
    in detectSpectrumAnomaliesPCA (preparePsdForPCA psd) threshold

-- | Detect anomalies using STFT results
detectAnomaliesSTFT :: [[Complex Double]] -- ^ STFT result
                    -> Double             -- ^ Threshold
                    -> [Anomaly]
detectAnomaliesSTFT stftResult threshold = 
    concatMap (detectAnomaliesInWindow threshold) (zip [0..] stftResult)

-- | Score anomalies based on window size or other criteria
scoreAnomalies :: [Anomaly] -- ^ List of anomalies
               -> Int       -- ^ Window size
               -> [Anomaly]
scoreAnomalies anomalies windowSize = 
    map (\a -> a { anomalyScore = computeScore a windowSize }) anomalies
  where
    computeScore :: Anomaly -> Int -> Double
    computeScore anomaly ws = fromIntegral ws * 0.1  -- Example scoring logic

-- | Prepare PSD data for PCA by organizing it into multiple observations
preparePsdForPCA :: [Double] -- ^ Power Spectral Density
                 -> [[Double]] -- ^ Prepared PSD for PCA
preparePsdForPCA psd = replicate 10 psd  -- Example: Replicating PSD for demonstration

-- | Detect spectrum anomalies using Principal Component Analysis (PCA)
-- This function applies PCA to the power spectral density (PSD) of the signal
-- and identifies anomalies based on the eigenvalues that exceed the threshold.
detectSpectrumAnomaliesPCA :: [[Double]] -- ^ Prepared PSD data
                           -> Double     -- ^ Threshold
                           -> [Anomaly]
detectSpectrumAnomaliesPCA psdList threshold =
    if null psdList || any null psdList
        then []
        else 
            let matrix = fromRows $ map fromList psdList
                maybePCA = performPCA matrix (min (LA.cols matrix) (LA.rows matrix - 1))
                significantEigenvalues = case maybePCA of
                    Nothing -> []
                    Just PrincipalComponents { eigenvalues } -> filter (> threshold) (toList eigenvalues)
            in [ Anomaly i ev SpectrumAnomaly 1.0 
               | (i, ev) <- zip [0..] significantEigenvalues
               ]

