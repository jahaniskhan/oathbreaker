{-# LANGUAGE NamedFieldPuns #-}

module ProactiveDefense.AdaptiveJamming
    ( adaptiveJamming
    , adaptiveJammingPCA
    ) where

import Data.Complex (Complex(..))
import Simulator.SignalGenerator
    ( generateInterferenceDeterministic
    , generateInterferenceStochastic
    , InterferenceType(..)
    )
import ThreatDetection.AnomalyDetection
import Numeric.LinearAlgebra (Vector, Matrix)
import qualified Numeric.LinearAlgebra as LA
import Utils.PCA (performPCA, PrincipalComponents(..))
import Utils.MathFunction (meanVector, stdVector)
import Control.Monad (unless)
import Control.Monad.Trans.Except
import Control.Monad.Except (liftEither, ExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified System.Random.MWC as MWC

-- | Perform adaptive jamming based on detected anomalies.
adaptiveJamming :: [Anomaly] -> Double -> Double -> IO (LA.Vector (Complex Double))
adaptiveJamming anomalies sampleRate duration = do
    -- Validate anomalies before processing
    let validAnomalies = filter validateAnomaly anomalies
    unless (null anomalies || length validAnomalies == length anomalies) $
        putStrLn "Warning: Some anomalies have invalid features and will be ignored."

    -- Create a new MWC generator
    gen <- MWC.createSystemRandom

    -- Generate interference vectors for each valid anomaly
    interferenceVectors <- mapM (\anomaly -> generateToneInterference gen anomaly sampleRate duration) validAnomalies

    -- Combine interference vectors safely
    let numSamples = round (sampleRate * duration)
        combinedInterference = if null interferenceVectors
                                then LA.konst 0 numSamples
                                else foldl1 (+) interferenceVectors

    return combinedInterference

-- | Perform adaptive jamming using PCA.
adaptiveJammingPCA :: Int -> [Anomaly] -> Double -> Double -> IO (Either String (LA.Vector (Complex Double)))
adaptiveJammingPCA numComponents anomalies sampleRate duration = runExceptT $ do
    let validAnomalies = filter validateAnomaly anomalies
    unless (length validAnomalies >= max 2 numComponents) $
        throwE "Not enough valid anomalies to perform PCA with the specified number of components."

    let featureVectors = map (LA.fromList . anomalyToFeatureVector) validAnomalies
        dataMatrix = LA.fromRows featureVectors

    standardizedData <- standardizeData dataMatrix
    let covarianceMatrix = computeCovarianceMatrix standardizedData

    PrincipalComponents { eigenvalues, eigenvectors } <- liftEither $ performPCA covarianceMatrix numComponents

    let principalComponents = LA.toColumns eigenvectors
        frequencyWeights = map (\pc -> pc LA.! 0) principalComponents

    -- Normalize and map frequencies
    let minW = minimum frequencyWeights
        maxW = maximum frequencyWeights
    unless (maxW - minW > 0) $
        throwE "Cannot normalize frequencies because all frequency weights are equal."

    let normalizedWeights = map (\w -> (w - minW) / (maxW - minW)) frequencyWeights
        maxFrequency = sampleRate / 2
        jammingFrequencies = map (* maxFrequency) normalizedWeights
        validFrequencies = filter (\f -> f > 0 && f <= maxFrequency) jammingFrequencies

    unless (not (null validFrequencies)) $
        throwE "No valid frequencies generated for interference."

    -- Initialize the random generator within the ExceptT monad
    gen <- liftIO MWC.createSystemRandom

    -- Generate interference signal using SpreadSpectrum
    interferenceVector <- liftIO $ generateInterferenceStochastic gen (SpreadSpectrum validFrequencies) sampleRate duration
    return interferenceVector

    where
        -- | Function to validate anomaly features
        validateAnomaly :: Anomaly -> Bool
        validateAnomaly Anomaly { anomalyFrequency, anomalyMagnitude, anomalyScore } =
            isFinite anomalyFrequency && anomalyFrequency > 0 &&
            isFinite anomalyMagnitude && anomalyMagnitude >= 0 &&
            isFinite anomalyScore && anomalyScore >= 0

        -- Helper function to check if a value is finite
        isFinite :: RealFloat a => a -> Bool
        isFinite x = not (isNaN x || isInfinite x)

        -- Generate a tone interference for a given anomaly
        generateToneInterference :: MWC.GenIO -> Anomaly -> Double -> Double -> IO (LA.Vector (Complex Double))
        generateToneInterference gen anomaly sampleRate duration = do
            let freq = anomalyFrequency anomaly
            let interferenceType = Tone freq
            let interference = generateInterferenceDeterministic interferenceType sampleRate duration
            return interference

        -- | Converts an anomaly to a feature vector.
        anomalyToFeatureVector :: Anomaly -> [Double]
        anomalyToFeatureVector Anomaly { anomalyFrequency, anomalyMagnitude, anomalyScore } =
            [ anomalyFrequency, anomalyMagnitude, anomalyScore ]

        -- | Standardize the data matrix (zero mean and unit variance)
        standardizeData :: LA.Matrix Double -> ExceptT String IO (LA.Matrix Double)
        standardizeData mat = do
            let means = meanVector mat
                centeredMat = mat - LA.asRow means
                stdDevs = stdVector mat
            -- Check that all standard deviations are non-zero
            unless (all (/= 0) (LA.toList stdDevs)) $
                throwE "Standard deviation is zero for one or more features; cannot standardize data."

            let standardizedMat = centeredMat / LA.asRow stdDevs
            return standardizedMat

        -- | Compute covariance matrix
        computeCovarianceMatrix :: LA.Matrix Double -> LA.Matrix Double
        computeCovarianceMatrix stdData =
            let n = fromIntegral (LA.rows stdData)
            in (LA.tr stdData LA.<> stdData) / (n - 1)
