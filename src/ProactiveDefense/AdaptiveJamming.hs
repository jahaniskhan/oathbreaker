module ProactiveDefense.AdaptiveJamming
    ( adaptiveJamming
    , adaptiveJammingPCA
    ) where

import Data.Complex (Complex(..))
import Simulator.SignalGenerator
import ThreatDetection.AnomalyDetection
import Numeric.LinearAlgebra (Vector, Matrix, (@>), (<>))
import qualified Numeric.LinearAlgebra as LA
import Utils.MathFunction (stddev)
import Utils.PCA (performPCA, PrincipalComponents(..))
import Control.Monad (unless)
import Control.Exception (try, SomeException)
import Data.Either (either)

-- | Perform adaptive jamming based on detected anomalies.
adaptiveJamming :: [Anomaly] -> Double -> Double -> IO (LA.Vector (Complex Double))
adaptiveJamming anomalies sampleRate duration = do
    -- Validate anomalies before processing
    let validAnomalies = filter validateAnomaly anomalies
    unless (null anomalies || length validAnomalies == length anomalies) $
        putStrLn "Warning: Some anomalies have invalid features and will be ignored."

    -- Generate interference vectors for each valid anomaly
    interferenceVectors <- mapM generateToneInterference validAnomalies

    -- Combine interference vectors safely
    let combinedInterference = if null interferenceVectors
                                then LA.fromList []
                                else LA.foldl1' (LA.zipWith (+)) interferenceVectors

    return combinedInterference
  where
    -- Function to validate anomaly features
    validateAnomaly :: Anomaly -> Bool
    validateAnomaly Anomaly { anomalyFrequency, anomalyMagnitude } =
        anomalyFrequency > 0 && anomalyMagnitude > 0

    -- Generate a tone interference for a given anomaly
    generateToneInterference :: Anomaly -> IO (LA.Vector (Complex Double))
    generateToneInterference anomaly = do
        let freq = anomalyFrequency anomaly
        interferenceList <- generateInterference (Tone freq) sampleRate duration
        return $ LA.fromList interferenceList

-- | Perform adaptive jamming using PCA based on detected anomalies.
adaptiveJammingPCA :: Int -> [Anomaly] -> Double -> Double -> IO (Either String [Complex Double])
adaptiveJammingPCA numComponents anomalies sampleRate duration = do
    let validAnomalies = filter validateAnomaly anomalies

    if length validAnomalies < max 2 numComponents
        then return $ Left "Not enough valid anomalies to perform PCA with the specified number of components."
        else do
            let dataMatrix = LA.fromRows $ map anomalyToFeatureVector validAnomalies
            case standardizeData dataMatrix of
                Left err -> return $ Left err
                Right standardizedData -> do
                    let covarianceMatrix = computeCovarianceMatrix standardizedData

                    -- Perform PCA
                    let pcaResult = performPCA covarianceMatrix numComponents
                    case pcaResult of
                        Left pcaErr -> return $ Left pcaErr
                        Right PrincipalComponents { eigenvectors } -> do
                            -- Map principal components to jamming frequencies meaningfully
                            let frequencyIndex = 0  -- Index of the frequency feature in the feature vector
                                scalingFactor = 1.0  -- Adjust based on your application's needs
                                principalComponents = LA.toColumns eigenvectors
                                frequencyWeights = map (\pc -> pc LA.@> frequencyIndex) principalComponents
                                jammingFrequencies = map (\w -> abs w * scalingFactor) frequencyWeights

                            -- Filter out non-positive frequencies
                            let validFrequencies = filter (> 0) jammingFrequencies
                            unless (length validFrequencies == length jammingFrequencies) $
                                putStrLn "Warning: Some jamming frequencies were non-positive and have been filtered out."

                            let jammingComplexFreqs = map (:+ 0) validFrequencies

                            -- Generate interference with error handling
                            interferenceResult <- generateInterferenceSafe (SpreadSpectrum jammingComplexFreqs) sampleRate duration
                            return interferenceResult
  where
    -- Validate anomaly features
    validateAnomaly :: Anomaly -> Bool
    validateAnomaly Anomaly { anomalyFrequency, anomalyMagnitude } =
        anomalyFrequency > 0 && anomalyMagnitude > 0

    -- Convert an anomaly to a feature vector for PCA
    anomalyToFeatureVector :: Anomaly -> [Double]
    anomalyToFeatureVector Anomaly { anomalyFrequency, anomalyMagnitude, anomalyScore } =
        [ anomalyFrequency
        , anomalyMagnitude
        , anomalyScore
        ]

    -- Standardize the data matrix (zero mean and unit variance)
    standardizeData :: LA.Matrix Double -> Either String (LA.Matrix Double)
    standardizeData mat =
        let means = LA.mean <$> LA.toColumns mat
            stdDevsList = map (stddev . LA.toList) $ LA.toColumns mat
            stdDevsRow = LA.asRow $ LA.fromList stdDevsList
            zerosInStdDevs = any (<= 0) stdDevsList
        in if zerosInStdDevs
            then Left "Standard deviation is zero or negative for one or more features; cannot standardize data."
            else
                let meansRow = LA.asRow $ LA.fromList means
                    standardizedMat = (mat - meansRow) / stdDevsRow
                in Right standardizedMat

    -- Compute covariance matrix
    computeCovarianceMatrix :: LA.Matrix Double -> LA.Matrix Double
    computeCovarianceMatrix stdData =
        let n = fromIntegral (LA.rows stdData)
        in (LA.tr stdData <> stdData) / (n - 1)

    -- Safely generate interference, handling potential errors
    generateInterferenceSafe :: InterferenceType -> Double -> Double -> IO (Either String [Complex Double])
    generateInterferenceSafe interferenceType sr dur = do
        result <- try $ generateInterference interferenceType sr dur :: IO (Either SomeException [Complex Double])
        return $ either (Left . show) Right result
