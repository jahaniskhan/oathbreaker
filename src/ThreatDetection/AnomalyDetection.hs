{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module ThreatDetection.AnomalyDetection
    ( detectAnomalies
    , Anomaly(..)
    , AnomalyType(..)
    ) where

import Data.Complex (Complex(..), magnitude)
import RFProcessor.SpectralAnalysis (calculatePowerSpectralDensity, detectPeaks)
import Numeric.LinearAlgebra (Vector)
import Numeric.GSL.Fourier (fft)
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector.Storable as V

-- Removed unused imports:
-- import Numeric.LinearAlgebra (Vector, fromList)
-- import Data.Vector.Storable (Vector)

-- Removed unused functions from Utils.MathFunction:
-- import Utils.MathFunction (median, medianAbsoluteDeviation)

import Utils.PCA (performPCA, PrincipalComponents(..))
import Utils.MathFunction (median, medianAbsoluteDeviation)

-- | Represents an anomaly in the signal
data Anomaly = Anomaly
    { anomalyFrequency :: Double      -- ^ Frequency in Hz
    , anomalyMagnitude :: Double
    , anomalyType      :: AnomalyType
    , anomalyScore     :: Double
    } deriving (Show, Eq)

-- | Types of anomalies
data AnomalyType = UnexpectedPeak 
                 | SignalDropout 
                 | NoiseBurst 
                 | SpectrumAnomaly 
                 | TimeFrequencyAnomaly
    deriving (Show, Eq)

-- | Create a Hamming window of the specified size
hammingWindow :: Int -> Vector Double
hammingWindow size = V.generate size $ \i ->
    let n = fromIntegral i
        m = fromIntegral size - 1
    in 0.54 - 0.46 * cos (2 * pi * n / m)

-- | Apply sliding window with Hamming function
slidingWindowWithHamming :: Int    -- ^ Window size
                         -> Int    -- ^ Hop size (overlap = window size - hop size)
                         -> [Complex Double]  -- ^ Input signal
                         -> [Vector (Complex Double)]  -- ^ List of windowed segments
slidingWindowWithHamming windowSize hopSize signal =
    let hamming = hammingWindow windowSize
        applyWindow segment = V.zipWith (\h x -> (h :+ 0) * x) hamming (V.fromList segment)
        go [] = []
        go xs
            | length xs < windowSize = []
            | otherwise = 
                let (window, rest) = splitAt windowSize xs
                    windowedSegment = applyWindow window
                in windowedSegment : go (drop hopSize xs)
    in go signal

-- | Apply Short-Time Fourier Transform to the signal with Hamming window
applySTFT :: [Complex Double] -- ^ Input signal
          -> Int              -- ^ Window size
          -> Int              -- ^ Hop size
          -> [Vector (Complex Double)] -- ^ List of FFT results per window
applySTFT signal windowSize hopSize =
    let windows = slidingWindowWithHamming windowSize hopSize signal
    in map fft windows

-- | Calculate the spectrogram of the signal
calculateSpectrogram :: [Complex Double] -> Int -> Int -> [[Double]]
calculateSpectrogram signal windowSize hopSize =
    let stftResult = applySTFT signal windowSize hopSize
    in map (map magnitude . V.toList) stftResult

-- | Detect time-frequency anomalies using the spectrogram
detectTimeFrequencyAnomalies :: [[Double]] -> Double -> Double -> Int -> [Anomaly]
detectTimeFrequencyAnomalies spectrogram threshold sampleRate windowSize =
    let numBins = windowSize `div` 2 + 1
        freqResolution = sampleRate / fromIntegral windowSize
    in [ Anomaly freq v TimeFrequencyAnomaly 1.0
       | (tIdx, row) <- zip [0..] spectrogram
       , (fIdx, v) <- zip [0..] row
       , fIdx < numBins
       , let freq = fromIntegral fIdx * freqResolution
       , v > threshold
       ]

-- | Main function to detect anomalies using various techniques
detectAnomalies :: [Complex Double] -- ^ Input signal
                -> Double           -- ^ Base threshold
                -> Int              -- ^ Score threshold
                -> Int              -- ^ Window size
                -> Int              -- ^ Hop size
                -> Double           -- ^ Sample rate
                -> [Anomaly]
detectAnomalies signal baseThreshold scoreThreshold windowSize hopSize sampleRate = 
    let psdList = calculatePowerSpectralDensity signal
        psd = LA.fromList psdList
        weightedThreshold = calculateWeightedThreshold psdList baseThreshold
        basicAnomalies = detectBasicAnomalies signal psd weightedThreshold sampleRate windowSize
        spectrogram = calculateSpectrogram signal windowSize hopSize
        timeFreqAnomalies = detectTimeFrequencyAnomalies spectrogram weightedThreshold sampleRate windowSize
        psdMatrix = preparePsdForPCA psdList
        pcaAnomalies = detectSpectrumAnomaliesPCA psdMatrix weightedThreshold sampleRate windowSize
        allAnomalies = basicAnomalies ++ timeFreqAnomalies ++ pcaAnomalies
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

-- | Detect basic anomalies (peaks, dropouts, bursts)
detectBasicAnomalies :: [Complex Double] -- ^ Input signal
                     -> LA.Vector Double -- ^ Power Spectral Density
                     -> Double           -- ^ Weighted threshold
                     -> Double           -- ^ Sample rate
                     -> Int              -- ^ Window size
                     -> [Anomaly]
detectBasicAnomalies signal psd threshold sampleRate windowSize = 
    let peaks = detectPeaks (LA.toList psd) 5
        avgMagnitude = if LA.size psd == 0 then 0 else LA.sumElements psd / fromIntegral (LA.size psd)
        localThresholds = calculateLocalThresholds (LA.toList psd)
        freqResolution = sampleRate / fromIntegral windowSize
        unexpectedPeaks = [ Anomaly freq v UnexpectedPeak 1.0
                          | (i, v) <- peaks
                          , let freq = fromIntegral i * freqResolution
                          , v > (localThresholds !! i) * avgMagnitude
                          ]
        signalDropouts = detectSignalDropouts signal avgMagnitude sampleRate
        noiseBursts = detectNoiseBursts signal avgMagnitude sampleRate
    in unexpectedPeaks ++ signalDropouts ++ noiseBursts

-- | Calculate local thresholds for peak detection
calculateLocalThresholds :: [Double] -> [Double]
calculateLocalThresholds psd = map (\v -> v * 1.5) psd  -- Example: Adjust factor as needed

-- | Detect signal dropouts based on average magnitude
detectSignalDropouts :: [Complex Double] -- ^ Input signal
                     -> Double           -- ^ Average magnitude
                     -> Double           -- ^ Sample rate
                     -> [Anomaly]
detectSignalDropouts signal avgMag sampleRate = 
    [ Anomaly freq v SignalDropout 1.0
    | (i, v) <- zip [0..] (map magnitude signal)
    , let freq = (fromIntegral i * sampleRate) / fromIntegral (length signal)
    , v < avgMag * 0.5  -- Example threshold
    ]

-- | Detect noise bursts based on average magnitude
detectNoiseBursts :: [Complex Double] -- ^ Input signal
                  -> Double           -- ^ Average magnitude
                  -> Double           -- ^ Sample rate
                  -> [Anomaly]
detectNoiseBursts signal avgMag sampleRate = 
    [ Anomaly freq v NoiseBurst 1.0
    | (i, v) <- zip [0..] (map magnitude signal)
    , let freq = (fromIntegral i * sampleRate) / fromIntegral (length signal)
    , v > avgMag * 2.0  -- Example threshold
    ]

-- | Score anomalies based on window size or other criteria
scoreAnomalies :: [Anomaly] -- ^ List of anomalies
               -> Int       -- ^ Window size
               -> [Anomaly]
scoreAnomalies anomalies windowSize = 
    map (\a -> a { anomalyScore = computeScore a windowSize }) anomalies
  where
    computeScore :: Anomaly -> Int -> Double
    computeScore anomaly ws = fromIntegral ws * 0.1  -- Example scoring logic

-- | Prepare PSD data for PCA by organizing it into a matrix
preparePsdForPCA :: [Double] -- ^ Power Spectral Density
                 -> LA.Matrix Double -- ^ Prepared PSD for PCA
preparePsdForPCA psd = LA.fromRows $ replicate 10 (LA.fromList psd)  -- Example: Replicating PSD for demonstration

-- | Detect spectrum anomalies using Principal Component Analysis (PCA)
detectSpectrumAnomaliesPCA :: LA.Matrix Double -- ^ Prepared PSD data
                           -> Double           -- ^ Threshold
                           -> Double           -- ^ Sample rate
                           -> Int              -- ^ Window size
                           -> [Anomaly]
detectSpectrumAnomaliesPCA psdMatrix threshold sampleRate windowSize =
    if LA.rows psdMatrix == 0 || LA.cols psdMatrix == 0
        then []
        else 
            let numBins = windowSize `div` 2 + 1
                freqResolution = sampleRate / fromIntegral windowSize
                numComponents = min (LA.cols psdMatrix) (LA.rows psdMatrix - 1)
                pcaResult = performPCA psdMatrix numComponents
            in case pcaResult of
                Left _ -> []
                Right PrincipalComponents { eigenvalues, eigenvectors } ->
                    let significantIndices = [ i | (i, ev) <- zip [0..] (LA.toList eigenvalues), ev > threshold ]
                        anomalies = [ Anomaly freq (eigenvalues LA.! i) SpectrumAnomaly 1.0
                                    | i <- significantIndices
                                    , i < numBins
                                    , let freq = fromIntegral i * freqResolution
                                    ]
                    in anomalies