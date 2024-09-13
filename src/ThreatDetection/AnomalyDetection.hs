{-# LANGUAGE OverloadedStrings #-}

module ThreatDetection.AnomalyDetection
    ( detectAnomalies
    , Anomaly(..)
    , AnomalyType(..)
    ) where

import Data.Complex (Complex(..), magnitude)
import RFProcessor.SpectralAnalysis (calculatePowerSpectralDensity, detectPeaks)
import qualified Data.Vector as V
import Numeric.LinearAlgebra (Matrix, Vector, fromList, toList, eigSH)
import qualified Numeric.LinearAlgebra as LA
import Data.List (transpose, sort)
import Statistics.Sample (median)
import Control.Monad (replicateM)
-- Import PCA from an external package or implement it manually
import qualified Numeric.LinearAlgebra.Devel as LA (pca, fft)

-- | Represents an anomaly in the signal
data Anomaly = Anomaly
    { anomalyFrequencyBin :: Int
    , anomalyMagnitude :: Double
    , anomalyType :: AnomalyType
    , anomalyScore :: Double  -- New field for anomaly score
    } deriving (Show)

-- | Types of anomalies
data AnomalyType = UnexpectedPeak | SignalDropout | NoiseBurst | SpectrumAnomaly | TimeFrequencyAnomaly deriving (Show)

-- | Main function to detect anomalies using various techniques
detectAnomalies :: [Complex Double] -> Double -> Int -> Int -> [Anomaly]
detectAnomalies signal baseThreshold scoreThreshold windowSize = 
    let psd = calculatePowerSpectralDensity signal
        stftResult = applySTFT signal 128
        weightedThreshold = calculateWeightedThreshold psd baseThreshold
        basicAnomalies = detectBasicAnomalies signal psd weightedThreshold
        stftAnomalies = detectAnomaliesSTFT stftResult weightedThreshold
        pcaAnomalies = detectSpectrumAnomaliesPCA psd weightedThreshold
        allAnomalies = basicAnomalies ++ stftAnomalies ++ pcaAnomalies
        scoredAnomalies = scoreAnomalies allAnomalies windowSize
    in filter (\a -> anomalyScore a > fromIntegral scoreThreshold) scoredAnomalies

-- | Calculate weighted threshold based on signal properties
calculateWeightedThreshold :: [Double] -> Double -> Double
calculateWeightedThreshold psd baseThreshold =
    let medianPSD = median psd
        madPSD = medianAbsoluteDeviation psd
    in baseThreshold * (medianPSD + 2 * madPSD)

-- | Calculate Median Absolute Deviation
medianAbsoluteDeviation :: [Double] -> Double
medianAbsoluteDeviation xs =
    let med = median xs
        deviations = map (\x -> abs (x - med)) xs
    in median deviations

-- | Detect basic anomalies (peaks, dropouts, bursts)
detectBasicAnomalies :: [Complex Double] -> [Double] -> Double -> [Anomaly]
detectBasicAnomalies signal psd threshold = 
    let peaks = detectPeaks psd 5
        avgMagnitude = sum psd / fromIntegral (length psd)
        localThresholds = calculateLocalThresholds psd
        unexpectedPeaks = [Anomaly i v UnexpectedPeak 1.0
                            | (i, v) <- peaks, 
                              v > (localThresholds !! i) * avgMagnitude]
        signalDropouts = detectSignalDropouts signal avgMagnitude
        noiseBursts = detectNoiseBursts signal avgMagnitude
    in unexpectedPeaks ++ signalDropouts ++ noiseBursts

-- | Apply STFT to a signal to analyze time-frequency characteristics
applySTFT :: [Complex Double] -> Int -> [[Complex Double]]
applySTFT signal windowSize =
    let windows = slidingWindows windowSize signal
    in map (LA.toList . LA.fft . LA.fromList) windows  -- Use fft from the correct module

-- | Detect anomalies using STFT analysis
detectAnomaliesSTFT :: [[Complex Double]] -> Double -> [Anomaly]
detectAnomaliesSTFT stftResult threshold = 
    concatMap (detectAnomaliesInWindow threshold) (zip [0..] stftResult)

-- | Detect anomalies within a time window's frequency spectrum
detectAnomaliesInWindow :: Double -> (Int, [Complex Double]) -> [Anomaly]
detectAnomaliesInWindow threshold (windowIndex, spectrum) = 
    let magnitudes = map magnitude spectrum
        avgMagnitude = sum magnitudes / fromIntegral (length magnitudes)
        peaks = detectPeaks magnitudes 5
    in [Anomaly (windowIndex * 100 + i) v TimeFrequencyAnomaly 1.0
        | (i, v) <- peaks, v > threshold * avgMagnitude]

-- | Detect spectrum anomalies using PCA
detectSpectrumAnomaliesPCA :: [Double] -> Double -> [Anomaly]
detectSpectrumAnomaliesPCA psd threshold =
    let -- Using PCA from hmatrix-pca or another library
        pcaResult = pca 1 (fromList psd)  -- Adjust parameters as needed
        eigenvalues = LA.toList $ LA.eigenvalues pcaResult
        anomalies = filter (\v -> v > threshold) eigenvalues
    in [Anomaly i v SpectrumAnomaly 1.0 | (i, v) <- zip [0..] anomalies]

-- | Score anomalies based on their occurrence within a sliding window
scoreAnomalies :: [Anomaly] -> Int -> [Anomaly]
scoreAnomalies anomalies windowSize =
    let windows = slidingWindows windowSize anomalies
        scoredWindows = map scoreWindow windows
    in concat scoredWindows

-- | Score anomalies within a single window
scoreWindow :: [Anomaly] -> [Anomaly]
scoreWindow window =
    let totalScore = fromIntegral (length window)
        scorePerAnomaly = if null window then 0 else totalScore / fromIntegral (length window)
    in map (\a -> a { anomalyScore = anomalyScore a + scorePerAnomaly }) window

-- | Calculate local thresholds based on a sliding window of local variance
calculateLocalThresholds :: [Double] -> [Double]
calculateLocalThresholds psd =
    let windowSize = 10
        variances = map variance (slidingWindows windowSize psd)
        paddedVariances = replicate (windowSize `div` 2) (head variances) ++ 
                          variances ++ 
                          replicate (windowSize `div` 2) (last variances)
    in map (\v -> sqrt v * 3) paddedVariances

-- | Utility function to calculate variance
variance :: [Double] -> Double
variance xs = 
    let meanX = sum xs / fromIntegral (length xs)
    in sum (map (\x -> (x - meanX) ** 2) xs) / fromIntegral (length xs)

-- | Utility function to generate sliding windows from a list
slidingWindows :: Int -> [a] -> [[a]]
slidingWindows n xs
    | length xs < n = []
    | otherwise     = take n xs : slidingWindows n (tail xs)

-- | Detect signal dropouts
detectSignalDropouts :: [Complex Double] -> Double -> [Anomaly]
detectSignalDropouts signal avgMagnitude =
    let dropoutThreshold = avgMagnitude * 0.1
        dropouts = filter (\(i, v) -> magnitude v < dropoutThreshold) (zip [0..] signal)
    in [Anomaly i (magnitude v) SignalDropout 1.0 | (i, v) <- dropouts]

-- | Detect noise bursts
detectNoiseBursts :: [Complex Double] -> Double -> [Anomaly]
detectNoiseBursts signal avgMagnitude =
    let burstThreshold = avgMagnitude * 5.0
        bursts = filter (\(i, v) -> magnitude v > burstThreshold) (zip [0..] signal)
    in [Anomaly i (magnitude v) NoiseBurst 1.0 | (i, v) <- bursts]
