{-# LANGUAGE ScopedTypeVariables #-}

module RFProcessor.SignalAcquisition (
    acquireSignal
) where

import Numeric.LinearAlgebra
import Control.Monad.Except (ExceptT(..), withExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Common.Types (AcquisitionConfig(..), SignalSource(..), AcquisitionError(..), SimulatedSignal(..))
import Control.Monad (when, replicateM)
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as E
import Data.Binary.Get (Get, runGetOrFail)
import Data.List (sortBy)
import Data.Function (on)
import Data.Complex (Complex(..))
import System.Random (randomRIO)
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra (Matrix, Vector, fromList, toList, eigSH)
import Data.Binary.Get (runGetOrFail, getDoublehost)
import Data.Ord (comparing)

-- | Performs Principal Component Analysis on the given data matrix.
-- Each row of the input matrix represents an observation,
-- and each column represents a feature.
performPCA :: LA.Matrix Double -> Int -> (LA.Matrix Double, LA.Vector Double)
performPCA dataMatrix numComponents = 
    let (eigenvalues, eigenvectors) = LA.eigSH $ LA.sym $ LA.tr dataMatrix LA.<> dataMatrix
        eigenPairs = zip (LA.toList eigenvalues) (LA.toColumns eigenvectors)
        sortedEigenPairs = sortBy (comparing (negate . fst)) eigenPairs
        (_, principalComponents) = unzip $ take numComponents sortedEigenPairs
        transformedData = dataMatrix LA.<> LA.fromColumns principalComponents
        explainedVariance = LA.fromList $ take numComponents $ LA.toList eigenvalues
    in (transformedData, explainedVariance)

-- | Sorts the indices of the vector in descending order based on the vector's values.
sortIndices :: Vector Double -> [Int]
sortIndices v = map snd $ sortBy (flip compare `on` fst) $ zip (toList v) [0..]

-- | Selects a subset of rows or columns based on the provided indices.
subMatrix :: [Int] -> [Int] -> LA.Matrix Double -> LA.Matrix Double
subMatrix rowIndices colIndices mat = 
    LA.fromLists [ [ mat `LA.atIndex` (r, c) | c <- colIndices ] | r <- rowIndices ]

-- | Acquire signal based on the configuration
acquireSignal :: AcquisitionConfig -> ExceptT AcquisitionError IO (LA.Matrix Double, LA.Vector Double)
acquireSignal config = case signalSource config of
    SDR -> do
        let deviceName = device config
        let filePath = path config
        liftIO $ putStrLn $ "Acquiring signal from SDR: " ++ deviceName
        fileContentsResult <- withExceptT (const SourceNotFound)
                                $ ExceptT (E.try (BL.readFile filePath) :: IO (Either E.IOException BL.ByteString))
        let numSamples = floor (sampleRate config * duration config) :: Int
        when (BL.length fileContentsResult < fromIntegral numSamples * 16) -- assuming Double is 8 bytes and Complex Double is 16 bytes
            $ throwError $ InvalidConfig "File does not contain enough samples"
        let parseResult = runGetOrFail (replicateM numSamples getComplexSample) fileContentsResult
        case parseResult of
            Left (_, _, errMsg) -> throwError $ ParsingFailed errMsg
            Right (_, _, samples) -> do
                let dataMatrix = fromLists [ [ realPart s, imagPart s ] | s <- samples ]  -- Assuming 2 features per sample
                let numComponents = 2
                let (transformedData, explainedVariance) = performPCA (LA.tr dataMatrix) numComponents
                return (transformedData, explainedVariance)
    Simulated simSignal -> do
        liftIO $ putStrLn "Generating simulated signal"
        simulatedSamples <- liftIO $ generateSimulatedSignal simSignal (sampleRate config) (duration config)
        let dataMatrix = LA.fromLists [map realPart simulatedSamples, map imagPart simulatedSamples]
        let numComponents = 2
        let (transformedData, explainedVariance) = performPCA (LA.tr dataMatrix) numComponents
        return (transformedData, explainedVariance)

-- | Parse a complex sample from binary data
getComplexSample :: Get (Complex Double)
getComplexSample = do
    real <- getDoublehost
    imag <- getDoublehost
    return (real :+ imag)

-- | Generate simulated signals based on the signal source
generateSimulatedSignal :: SimulatedSignal -> Double -> Double -> IO [Complex Double]
generateSimulatedSignal simSignal sampleRate duration = case simSignal of
    WhiteNoise -> generateWhiteNoise sampleRate duration
    Sine freq  -> generateSine freq sampleRate duration
    MultiTone freqs -> generateMultiTone freqs sampleRate duration
    QPSK symbolRate -> generateQPSK symbolRate sampleRate duration

-- | Generate white noise samples
generateWhiteNoise :: Double -> Double -> IO [Complex Double]
generateWhiteNoise sampleRate duration = do
    let numSamples = floor (sampleRate * duration)
    realParts <- replicateM numSamples (randomRIO (-1.0, 1.0))
    imagParts <- replicateM numSamples (randomRIO (-1.0, 1.0))
    return $ zipWith (:+) realParts imagParts

-- | Generate a sine wave
generateSine :: Double -> Double -> Double -> IO [Complex Double]
generateSine freq sampleRate duration = do
    let numSamples = floor (sampleRate * duration)
    let t = [0, 1 / sampleRate .. duration - 1 / sampleRate]
    return [cos (2 * pi * freq * ti) :+ sin (2 * pi * freq * ti) | ti <- t]

-- | Generate multi-tone signal
generateMultiTone :: [Double] -> Double -> Double -> IO [Complex Double]
generateMultiTone freqs sampleRate duration = do
    let numSamples = floor (sampleRate * duration)
    let t = [0, 1 / sampleRate .. duration - 1 / sampleRate]
    return [sum [cos (2 * pi * f * ti) :+ sin (2 * pi * f * ti) | f <- freqs] | ti <- t]

-- | Generate QPSK signal
generateQPSK :: Double -> Double -> Double -> IO [Complex Double]
generateQPSK symbolRate sampleRate duration = do
    let numSymbols = floor (symbolRate * duration)
        samplesPerSymbol = floor (sampleRate / symbolRate)
    symbols <- replicateM numSymbols randomQPSKSymbol
    return $ concatMap (replicate samplesPerSymbol) symbols

-- | Generate a random QPSK symbol
randomQPSKSymbol :: IO (Complex Double)
randomQPSKSymbol = do
    bit1 <- randomRIO (0, 1) :: IO Int
    bit2 <- randomRIO (0, 1) :: IO Int
    let symbol = case (bit1, bit2) of
            (0, 0) -> 1 :+ 1
            (0, 1) -> (-1) :+ 1
            (1, 0) -> 1 :+ (-1)
            (1, 1) -> (-1) :+ (-1)
    return symbol

-- Local definitions for cov and fromSymmetric
cov :: LA.Matrix Double -> LA.Matrix Double
cov matrix =
    let centered = LA.cmap (\x -> x - LA.sumElements matrix / fromIntegral (LA.size matrix)) matrix
        n = fromIntegral (LA.rows matrix - 1)
    in (LA.tr centered LA.<> centered) / n

fromSymmetric :: LA.Matrix Double -> LA.Matrix Double
fromSymmetric m = (m + LA.tr m) / 2
