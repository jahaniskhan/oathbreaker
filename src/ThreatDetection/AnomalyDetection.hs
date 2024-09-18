{-# LANGUAGE OverloadedStrings #-}

module ThreatDetection.AnomalyDetection
    ( detectAnomalies
    , Anomaly(..)
    , AnomalyType(..)
    ) where

import Data.Complex (Complex(..), magnitude)
import RFProcessor.SpectralAnalysis (calculatePowerSpectralDensity, detectPeaks)
import Numeric.LinearAlgebra (Matrix, Vector, fromList, eigSH, sym)
import Math.FFT (dft)
import qualified Numeric.LinearAlgebra as LA
import Control.Monad (replicateM)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

import Utils.MathFunctions (median, medianAbsoluteDeviation)

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
detectAnomalies :: [Complex Double] -> Double -> Int -> Int -> [Anomaly]
detectAnomalies signal baseThreshold scoreThreshold windowSize = 
    let psd               = calculatePowerSpectralDensity signal
        stftResult        = applySTFT signal 128
        weightedThreshold = calculateWeightedThreshold psd baseThreshold
        basicAnomalies    = detectBasicAnomalies signal psd weightedThreshold
        stftAnomalies     = detectAnomaliesSTFT stftResult weightedThreshold
        pcaAnomalies      = detectSpectrumAnomaliesPCA psd weightedThreshold
        allAnomalies      = basicAnomalies ++ stftAnomalies ++ pcaAnomalies
        scoredAnomalies   = scoreAnomalies allAnomalies windowSize
    in filter (\a -> anomalyScore a > fromIntegral scoreThreshold) scoredAnomalies

-- | Calculate weighted threshold based on signal properties
calculateWeightedThreshold :: [Double] -> Double -> Double
calculateWeightedThreshold psd baseThreshold =
    let medianPSD = median psd
        madPSD    = medianAbsoluteDeviation psd
    in baseThreshold * (medianPSD + 2 * madPSD)

-- | Detect basic anomalies (peaks, dropouts, bursts)
detectBasicAnomalies :: [Complex Double] -> [Double] -> Double -> [Anomaly]
detectBasicAnomalies signal psd threshold = 
    let peaks          = detectPeaks psd 5
        avgMagnitude   = sum psd / fromIntegral (length psd)
        localThresholds = calculateLocalThresholds psd
        unexpectedPeaks = [ Anomaly i v UnexpectedPeak 1.0
                          | (i, v) <- peaks
                          , v > (localThresholds !! i) * avgMagnitude
                          ]
        signalDropouts = detectSignalDropouts signal avgMagnitude
        noiseBursts    = detectNoiseBursts signal avgMagnitude
    in unexpectedPeaks ++ signalDropouts ++ noiseBursts

-- | Apply Short-Time Fourier Transform to the signal.
applySTFT :: [Complex Double] -> Int -> [[Complex Double]]
applySTFT signal windowSize =
    let windows = slidingWindows windowSize signal
    in map (V.toList . dft . V.fromList) windows  -- Use 'dft' instead of 'fft'

-- | Detect anomalies using STFT analysis
detectAnomaliesSTFT :: [[Complex Double]] -> Double -> [Anomaly]
detectAnomaliesSTFT stftResult threshold = 
    concatMap (detectAnomaliesInWindow threshold) (zip [0..] stftResult)

-- | Detect anomalies within a time window's frequency spectrum
detectAnomaliesInWindow :: Double -> (Int, [Complex Double]) -> [Anomaly]
detectAnomaliesInWindow threshold (windowIndex, spectrum) = 
    let magnitudes     = map magnitude spectrum
        avgMagnitude   = sum magnitudes / fromIntegral (length magnitudes)
        peaks          = detectPeaks magnitudes 5
    in [ Anomaly (windowIndex * 100 + i) v TimeFrequencyAnomaly 1.0
       | (i, v) <- peaks
       , v > threshold * avgMagnitude
       ]

-- | Detect spectrum anomalies using Principal Component Analysis (PCA)
-- This function applies PCA to the power spectral density (PSD) of the signal
-- and identifies anomalies based on the eigenvalues that exceed the threshold.
detectSpectrumAnomaliesPCA :: [Double] -> Double -> [Anomaly]
detectSpectrumAnomaliesPCA psd threshold =
    let matrix            = LA.fromList [psd]
        covarianceMatrix  = calculateCovarianceMatrix matrix
        (eigenvalues, _)  = eigSH (sym covarianceMatrix)
        significantEigenvalues = filter (> threshold) (LA.toList eigenvalues)
    in [ Anomaly i ev SpectrumAnomaly 1.0 
       | (i, ev) <- zip [0..] significantEigenvalues
       ]

-- | Calculate covariance matrix for PCA
calculateCovarianceMatrix :: Matrix Double -> Matrix Double
calculateCovarianceMatrix matrix =
    let centered = LA.cmap (\x -> x - LA.mean matrix) matrix
        n        = fromIntegral (LA.rows matrix - 1)
    in (LA.tr centered LA.<> centered) / n

-- | Utility function to calculate local thresholds (placeholder)
calculateLocalThresholds :: [Double] -> [Double]
calculateLocalThresholds psd = replicate (length psd) 1.0  -- Replace with actual implementation

-- | Placeholder function to score anomalies
scoreAnomalies :: [Anomaly] -> Int -> [Anomaly]
scoreAnomalies anomalies windowSize = anomalies  -- Replace with actual scoring logic
