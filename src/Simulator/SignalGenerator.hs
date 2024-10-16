{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simulator.SignalGenerator (
    generateBaseSignal,
    injectSignature,
    injectAnomaly,
    generateWhiteNoise,
    addNoise,
    generateCompositeSignal,
    generateInterferenceDeterministic,
    generateInterferenceStochastic,
    InterferenceType(..),
    Signature(..),
    AnomalyPattern(..),
    Signal(..),
    injectSignatureRandom,
    injectAnomalyRandom
) where

import Common.Types
import Data.Complex (Complex(..), cis)
import Control.Monad (replicateM, foldM)
import Data.List (splitAt, foldl1)
import qualified Data.Vector.Storable as V
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC
import Control.Monad.Primitive (PrimState)
import System.Random (RandomGen, randomR)
import Numeric.LinearAlgebra (Vector, fromList)
import qualified Numeric.LinearAlgebra as LA

-- | Represents a signal with its sampling rate and samples.
data Signal = Signal
    { signalSampleRate :: Double
    , samples    :: V.Vector (Complex Double)
    } deriving (Show, Eq)

-- | Represents a signature with a name and pattern.
data Signature = Signature
    { signatureName    :: String
    , signaturePattern :: [Complex Double]
    } deriving (Show, Eq)

-- | Represents an anomaly pattern with its characteristics.
data AnomalyPattern = AnomalyPattern
    { anomalyPattern  :: [Complex Double]
    , anomalyFrequency :: Double
    } deriving (Show, Eq)

-- | Enumerates different types of interference.
data InterferenceType
    = WhiteNoise
    | Tone Double
    | Sweep Double Double
    | QAM Int Double
    | OFDM [Double]
    | Chirp Double Double
    | SpreadSpectrum [Double]
    | MultiTone [Double]
    deriving (Show, Eq)

-- | Generates the base sine wave signal.
generateBaseSignal :: Int -> Double -> Double -> Double -> Signal
generateBaseSignal numSamples baseFreq sampleRate amplitude =
    let time = [0..fromIntegral numSamples - 1] / sampleRate
        sig = map (\t -> amplitude * cis (2 * pi * baseFreq * t)) time
    in Signal sampleRate (V.fromList sig)

-- | Injects a signature into the signal at a random position.
injectSignatureRandom :: GenIO -> Signature -> V.Vector (Complex Double) -> IO (V.Vector (Complex Double))
injectSignatureRandom gen Signature { signaturePattern } samples = do
    let sigLen = V.length samples
        patLen = length signaturePattern
    pos <- MWC.uniformR (0, sigLen - patLen) gen
    let updated = samples V.// [(pos + i, samples V.! (pos + i) + signaturePattern !! i) | i <- [0..patLen -1]]
    return updated

-- | Injects an anomaly into the signal at a specified position.
injectAnomaly :: Int -> AnomalyPattern -> V.Vector (Complex Double) -> V.Vector (Complex Double)
injectAnomaly pos AnomalyPattern { anomalyPattern } samples =
    let patLen = length anomalyPattern
        updated = samples V.// [(pos + i, samples V.! (pos + i) + anomalyPattern !! i) | i <- [0..patLen -1], pos + i < V.length samples]
    in updated

-- | Injects an anomaly at a random position.
injectAnomalyRandom :: GenIO -> AnomalyPattern -> V.Vector (Complex Double) -> IO (V.Vector (Complex Double))
injectAnomalyRandom gen anomalyPattern samples = do
    let sigLen = V.length samples
        patLen = length (anomalyPattern anomalyPattern)
    pos <- MWC.uniformR (0, sigLen - patLen) gen
    return $ injectAnomaly pos anomalyPattern samples

-- | Generates white noise.
generateWhiteNoise :: GenIO -> Int -> Double -> IO (V.Vector (Complex Double))
generateWhiteNoise gen numSamples amplitude = do
    noise <- V.replicateM numSamples $ do
        real <- MWC.normal 0 (amplitude / sqrt 2) gen
        imag <- MWC.normal 0 (amplitude / sqrt 2) gen
        return (real :+ imag)
    return noise

-- | Adds two signals together.
addNoise :: V.Vector (Complex Double) -> V.Vector (Complex Double) -> V.Vector (Complex Double)
addNoise = V.zipWith (+)

-- | Generates a composite signal with signatures and anomalies.
generateCompositeSignal :: GenIO -> Int -> Double -> Double -> [Signature] -> [AnomalyPattern] -> Double -> IO Signal
generateCompositeSignal gen numSamples baseFreq sampleRate signatures anomalies noiseAmp = do
    -- Step 1: Generate the base signal
    let baseSignal = generateBaseSignal numSamples baseFreq sampleRate 1.0
        baseSamples = samples baseSignal
    
    -- Step 2: Inject signatures
    sigSamples <- foldM (\acc sig -> injectSignatureRandom gen sig acc) baseSamples signatures
    
    -- Step 3: Inject anomalies
    finalSamples <- foldM (\acc anomaly -> injectAnomalyRandom gen anomaly acc) sigSamples anomalies
    
    -- Step 4: Add overall noise
    whiteNoise <- generateWhiteNoise gen numSamples noiseAmp
    let noisySamples = addNoise finalSamples whiteNoise
    
    -- Step 5: Return the final composite signal
    return $ Signal sampleRate noisySamples

-- | Generates deterministic interference based on InterferenceType.
generateInterferenceDeterministic :: InterferenceType -> Double -> Double -> IO (LA.Vector (Complex Double))
generateInterferenceDeterministic (Tone freq) sampleRate duration = do
    let numSamples = floor (sampleRate * duration)
        time = [0..fromIntegral numSamples - 1] / sampleRate
        interference = map (\t -> cis (2 * pi * freq * t)) time
    return $ LA.fromList interference
generateInterferenceDeterministic (Sweep startFreq endFreq) sampleRate duration = do
    let numSamples = floor (sampleRate * duration)
        time = [0..fromIntegral numSamples - 1] / sampleRate
        interference = map (\t -> cis (2 * pi * (startFreq + (endFreq - startFreq) * t / duration) * t)) time
    return $ LA.fromList interference
generateInterferenceDeterministic (MultiTone freqs) sampleRate duration = do
    let numSamples = floor (sampleRate * duration)
        time = [0..fromIntegral numSamples - 1] / sampleRate
        interference = [ sum [cis (2 * pi * f * t) | f <- freqs] | t <- time ]
    return $ LA.fromList interference
-- Add other deterministic interference types as needed.

-- | Generates stochastic interference based on InterferenceType.
generateInterferenceStochastic :: GenIO -> InterferenceType -> Double -> Double -> IO (LA.Vector (Complex Double))
generateInterferenceStochastic gen WhiteNoise sampleRate duration = do
    noise <- generateWhiteNoise gen (floor $ sampleRate * duration) 1.0
    return $ LA.fromList $ V.toList noise
generateInterferenceStochastic gen (SpreadSpectrum freqs) sampleRate duration = do
    let numSamples = floor (sampleRate * duration)
    spreadSignal <- foldM (\acc f -> do
                            interference <- generateInterferenceDeterministic (Tone f) sampleRate duration
                            return $ acc + interference
                        ) (LA.konst 0 numSamples) freqs
    return spreadSignal
-- Add other stochastic interference types like OFDM, QAM, Chirp as needed.

-- | Utility function to perform foldM with Vectors.
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM _ acc []     = return acc
foldM f acc (x:xs) = f acc x >>= \acc' -> foldM f acc' xs

-- Additional Interference Generators

-- | Generates QAM interference.
generateQAM :: MWC.GenIO -> Int -> Double -> Double -> Double -> IO (V.Vector (Complex Double))
generateQAM gen order symbolRate sr duration = do
    let numSymbols = floor (duration * symbolRate) :: Int
        symbolValues = [mkQAMSymbol order i | i <- [0 .. order - 1]]
    symbols <- replicateM numSymbols (MWC.uniformR (0, length symbolValues - 1) gen)
    let qamSignal = [symbolValues !! s | s <- symbols]
        samplesPerSymbol = floor (sr / symbolRate) :: Int
        interpolatedSignal = concatMap (replicate samplesPerSymbol) qamSignal
    return $ V.fromList $ take (floor (sr * duration)) interpolatedSignal

-- | Generate a QAM symbol given the order and symbol index.
mkQAMSymbol :: Int -> Int -> Complex Double
mkQAMSymbol order idx =
    let m = floor (sqrt (fromIntegral order :: Double)) :: Int
        i = fromIntegral (idx `mod` m) - (fromIntegral m - 1) / 2
        q = fromIntegral (idx `div` m) - (fromIntegral m - 1) / 2
    in i :+ q

-- | Generates an OFDM interference signal.
generateOFDM :: MWC.GenIO -> [Double] -- ^ Subcarrier frequencies
             -> Double       -- ^ OFDM sample rate in Hz
             -> Double       -- ^ Duration in seconds
             -> IO (V.Vector (Complex Double))
generateOFDM gen subcarriers sr duration = do
    let numSamples = floor (sr * duration) :: Int
        t = [0, 1 / sr .. duration - 1 / sr]
    -- Generate random phase for each subcarrier
    phases <- replicateM (length subcarriers) $ MWC.uniformR (0, 2 * pi) gen
    return $ V.fromList [sum [cos (2 * pi * f * ti + phase) :+ sin (2 * pi * f * ti + phase) | (f, phase) <- zip subcarriers phases] | ti <- take numSamples t]

-- | Generates a Chirp interference signal.
generateChirp :: MWC.GenIO -> Double -- ^ Start frequency in Hz
              -> Double -- ^ End frequency in Hz
              -> Double -- ^ Chirp sample rate in Hz
              -> Double -- ^ Duration in seconds
              -> IO (V.Vector (Complex Double))
generateChirp gen startFreq endFreq sr duration = do
    let numSamples = floor (sr * duration) :: Int
        t = [0, 1 / sr .. duration - 1 / sr]
        k = (endFreq - startFreq) / duration
    -- Example: Introduce random phase for chirp
    phase <- MWC.uniformR (0, 2 * pi) gen
    return $ V.fromList [cos (2 * pi * (startFreq * ti + k * ti * ti / 2) + phase) :+ sin (2 * pi * (startFreq * ti + k * ti * ti / 2) + phase) | ti <- take numSamples t]

-- | Generates Spread Spectrum interference.
generateSpreadSpectrum :: MWC.GenIO -> [Double] -- ^ Spread Spectrum frequencies
                        -> Double -- ^ Spread Spectrum sample rate in Hz
                        -> Double -- ^ Duration in seconds
                        -> IO (V.Vector (Complex Double))
generateSpreadSpectrum gen freqs sr duration = do
    let t = [0, 1 / sr .. duration - 1 / sr]
    -- Example: Introduce random phase per frequency
    phases <- replicateM (length freqs) $ MWC.uniformR (0, 2 * pi) gen
    return $ V.fromList [sum [cos (2 * pi * f * ti + phase) :+ sin (2 * pi * f * ti + phase) | (f, phase) <- zip freqs phases] | ti <- t]

-- | Generates a stochastic multi-tone interference signal by adding random phases to each tone.
generateMultiToneStochastic :: MWC.GenIO -> [Double] -- ^ Frequencies
                             -> Double -- ^ Sample rate in Hz
                             -> Double -- ^ Duration in seconds
                             -> IO (V.Vector (Complex Double))
generateMultiToneStochastic gen frequencies sr duration = do
    let numSamples = floor (sr * duration) :: Int
        t = [0, 1 / sr .. duration - 1 / sr]
    -- For each frequency, generate a list of samples with a random phase
    interference <- mapM (\f -> do
                             phase <- MWC.uniformR (0, 2 * pi) gen
                             return [cos (2 * pi * f * ti + phase) :+ sin (2 * pi * f * ti + phase) | ti <- t]
                         ) frequencies
    -- Sum all interference vectors element-wise
    let summedInterference = foldl1 (+) interference
    return $ LA.fromList summedInterference
