module ProactiveDefense.AdaptiveJamming
    ( adaptiveJamming
    , adaptiveJammingPCA
    ) where

import Data.Complex
import Simulator.SignalGenerator
import ThreatDetection.AnomalyDetection
import Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra as LA

-- | Perform adaptive jamming based on detected anomalies
adaptiveJamming :: [Anomaly] -> Double -> Double -> IO [Complex Double]
adaptiveJamming anomalies sampleRate duration =
    concatMap (\anomaly -> generateInterference (Tone (anomalyFrequencyBin anomaly * sampleRate / fromIntegral (length anomalies))) sampleRate duration) anomalies

adaptiveJammingPCA :: [Anomaly] -> Double -> Double -> IO [Complex Double]
adaptiveJammingPCA anomalies sampleRate duration = do
    let matrix = LA.fromLists [map anomalyMagnitude anomalies]
        eigResult = LA.eigSH matrix
        eigenvalues = LA.eigenvalues eigResult
        eigenvectors = LA.eigenvectors eigResult
        principalComponents = take 3 $ LA.toColumns eigenvectors
        jammingFrequencies = concatMap (map (* sampleRate)) principalComponents
    generateInterference (SpreadSpectrum jammingFrequencies) sampleRate duration
