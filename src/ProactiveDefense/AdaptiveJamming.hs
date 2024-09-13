module ProactiveDefense.AdaptiveJamming
    ( adaptiveJamming
    , adaptiveJammingPCA
    ) where

import Data.Complex
import ProactiveDefense.InterferenceGeneration
import ThreatDetection.AnomalyDetection
import Numeric.LinearAlgebra
import qualified Data.Vector as V

-- | Perform adaptive jamming based on detected anomalies
adaptiveJamming :: [Anomaly] -> Double -> Double -> IO [Complex Double]
adaptiveJamming anomalies sampleRate duration =
    concatMap (\anomaly -> generateInterference (Tone (fromIntegral (anomalyFrequencyBin anomaly) * sampleRate / fromIntegral (length anomalies))) sampleRate duration) anomalies

-- | Perform adaptive jamming based on detected anomalies using PCA
adaptiveJammingPCA :: [Anomaly] -> Double -> Double -> IO [Complex Double]
adaptiveJammingPCA anomalies sampleRate duration = do
    let matrix = fromLists [map anomalyMagnitude anomalies]
        (eigenvalues, eigenvectors) = eig matrix
        principalComponents = take 3 $ toColumns eigenvectors
        jammingFrequencies = concatMap (V.toList . scale sampleRate) principalComponents
    generateInterference (SpreadSpectrum jammingFrequencies) sampleRate duration

-- | Generate spread spectrum interference
generateSpreadSpectrum :: [Double] -> Double -> Double -> IO [Complex Double]
generateSpreadSpectrum freqs sampleRate duration = do
    let t = [0, 1 / sampleRate .. duration - 1 / sampleRate]
        signal = [sum [cos (2 * pi * f * ti) :+ sin (2 * pi * f * ti) | f <- freqs] | ti <- t]
    return signal
