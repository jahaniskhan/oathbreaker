module Simulator.SignalGenerator
    ( generateBaseSignal
    , injectSignature
    , injectAnomaly
    , generateWhiteNoise
    , addNoise
    , generateCompositeSignal
    , generateInterference
    , InterferenceType(..)
    , Signature(..)
    , AnomalyPattern(..)
    , Signal(..)  -- Exporting the Signal data type
    ) where

import Data.Complex (Complex(..), cis)
import Numeric.LinearAlgebra (Vector, fromList, toList, fromRows)
import System.Random.MWC (createSystemRandom, uniformR)
import System.Random.MWC.Distributions (normal)
import Control.Monad (replicateM, foldM)
import Data.List (splitAt)
import Data.Bifunctor (first)
import qualified Data.Vector.Storable as V

-- | Represents a signal with metadata
data Signal = Signal
    { sampleRate :: Double             -- ^ Sample rate in Hz
    , samples    :: V.Vector (Complex Double) -- ^ Signal samples
    } deriving (Show, Eq)

-- | Represents a known signal signature
data Signature = Signature
    { signatureName    :: String
    , signaturePattern :: [Complex Double]
    } deriving (Show, Eq)

-- | Represents an anomaly pattern with position.
data AnomalyPattern = AnomalyPattern
    { anomalyPattern :: [Complex Double]
    , anomalyPos     :: Int
    } deriving (Show, Eq)

-- | Types of interference that can be generated
data InterferenceType
    = WhiteNoise
    | Tone Double              -- ^ Single frequency tone
    | Sweep Double Double      -- ^ Frequency sweep from start to end frequency
    | QAM Int Double           -- ^ QAM with specified order and symbol rate
    | OFDM [Double]            -- ^ OFDM with specified subcarrier frequencies
    | Chirp Double Double      -- ^ Chirp signal from start to end frequency
    | SpreadSpectrum [Double]  -- ^ Spread Spectrum with specified frequencies
    deriving (Show)

-- | Generates a base signal, e.g., a sine wave with given frequency and sample rate.
generateBaseSignal :: Int        -- ^ Number of samples
                  -> Double     -- ^ Frequency in Hz
                  -> Double     -- ^ Sample rate in Hz
                  -> Signal
generateBaseSignal sampleCount frequency sampleRate =
    let signalSamples = [ cis (2 * pi * frequency * t / sampleRate) | t <- [0..fromIntegral sampleCount - 1] ]
    in Signal sampleRate (V.fromList signalSamples)

-- | Injects a known signature into the signal at a specified position.
injectSignature :: [Complex Double]    -- ^ Signature pattern
                -> Int                  -- ^ Position to inject the signature
                -> V.Vector (Complex Double)     -- ^ Original signal
                -> V.Vector (Complex Double)     -- ^ Signal with injected signature
injectSignature signature pos signal =
    let (before, after) = V.splitAt pos signal
        signatureVector = V.fromList signature
    in before V.++ signatureVector V.++ V.drop (pos + V.length signatureVector) after

-- | Injects an anomaly into the signal.
injectAnomaly :: [Complex Double]    -- ^ Anomaly pattern
              -> Int                  -- ^ Position to inject the anomaly
              -> V.Vector (Complex Double)     -- ^ Original signal
              -> V.Vector (Complex Double)     -- ^ Signal with injected anomaly
injectAnomaly anomaly pos signal =
    let (before, after) = V.splitAt pos signal
        anomalyVector = V.fromList anomaly
    in before V.++ anomalyVector V.++ V.drop (pos + V.length anomalyVector) after

-- | Generates random white noise to be added to the signal.
generateWhiteNoise :: Int        -- ^ Number of samples
                   -> Double     -- ^ Noise amplitude
                   -> IO (V.Vector (Complex Double))
generateWhiteNoise numSamples amplitude = do
    gen <- createSystemRandom
    noiseList <- replicateM numSamples $ do
        realPart <- normal 0 (amplitude / sqrt 2) gen
        imagPart <- normal 0 (amplitude / sqrt 2) gen
        return (realPart :+ imagPart)
    return $ V.fromList noiseList

-- | Combines base signal with noise.
addNoise :: V.Vector (Complex Double)  -- ^ Base signal
         -> V.Vector (Complex Double)  -- ^ Noise
         -> V.Vector (Complex Double)
addNoise = V.zipWith (+)

-- | Generates an interference signal based on the specified InterferenceType.
generateInterference :: InterferenceType -- ^ Type of interference to generate
                     -> Double           -- ^ Sample rate in Hz
                     -> Double           -- ^ Duration in seconds
                     -> IO (V.Vector (Complex Double))
generateInterference WhiteNoise sampleRate duration = do
    let numSamples = floor (sampleRate * duration)
    whiteNoise <- generateWhiteNoise numSamples 1.0
    return whiteNoise

generateInterference (Tone frequency) sampleRate duration = do
    let numSamples = floor (sampleRate * duration)
        t = [0, 1 / sampleRate .. duration - 1 / sampleRate]
    return $ V.fromList [cos (2 * pi * frequency * ti) :+ 0 | ti <- take numSamples t]

generateInterference (Sweep startFreq endFreq) sampleRate duration = do
    let numSamples = floor (sampleRate * duration)
        t = [0, 1 / sampleRate .. duration - 1 / sampleRate]
        freqs = [startFreq + (endFreq - startFreq) * ti / duration | ti <- take numSamples t]
    return $ V.fromList [cos (2 * pi * f * ti) :+ 0 | (f, ti) <- zip freqs t]

generateInterference (QAM order symbolRate) sampleRate duration = 
    generateQAM order symbolRate sampleRate duration

generateInterference (OFDM subcarriers) sampleRate duration = 
    generateOFDM subcarriers sampleRate duration

generateInterference (Chirp startFreq endFreq) sampleRate duration = 
    generateChirp startFreq endFreq sampleRate duration

generateInterference (SpreadSpectrum freqs) sampleRate duration = 
    generateSpreadSpectrum freqs sampleRate duration

-- | Generate QAM signal
generateQAM :: Int -> Double -> Double -> Double -> IO (V.Vector (Complex Double))
generateQAM order symbolRate sampleRate duration = do
    gen <- createSystemRandom
    let numSymbols = floor (duration * symbolRate)
        symbolValues = [ mkQAMSymbol order i | i <- [0 .. order - 1] ]
    symbols <- replicateM numSymbols (uniformR (0, length symbolValues - 1) gen)
    let qamSignal = [symbolValues !! s | s <- symbols]
        samplesPerSymbol = floor (sampleRate / symbolRate)
        interpolatedSignal = concatMap (replicate samplesPerSymbol) qamSignal
    return $ V.fromList $ take (floor (sampleRate * duration)) interpolatedSignal

-- | Generate a QAM symbol given the order and symbol index
mkQAMSymbol :: Int -> Int -> Complex Double
mkQAMSymbol order idx =
    let m = floor (sqrt (fromIntegral order :: Double)) :: Int
        i = fromIntegral (idx `mod` m) - (fromIntegral m - 1) / 2
        q = fromIntegral (idx `div` m) - (fromIntegral m - 1) / 2
    in i :+ q

-- | Generate OFDM signal
generateOFDM :: [Double] -> Double -> Double -> IO (V.Vector (Complex Double))
generateOFDM subcarriers sampleRate duration = do
    let numSamples = floor (sampleRate * duration)
        t = [0, 1 / sampleRate .. duration - 1 / sampleRate]
    return $ V.fromList [sum [cos (2 * pi * f * ti) :+ sin (2 * pi * f * ti) | f <- subcarriers] | ti <- take numSamples t]

-- | Generate Chirp signal
generateChirp :: Double -> Double -> Double -> Double -> IO (V.Vector (Complex Double))
generateChirp startFreq endFreq sampleRate duration = do
    let numSamples = floor (sampleRate * duration)
        t = [0, 1 / sampleRate .. duration - 1 / sampleRate]
        k = (endFreq - startFreq) / duration
    return $ V.fromList [cos (2 * pi * (startFreq * ti + k * ti * ti / 2)) :+ sin (2 * pi * (startFreq * ti + k * ti * ti / 2)) | ti <- take numSamples t]

-- | Generate Spread Spectrum interference
generateSpreadSpectrum :: [Double] -> Double -> Double -> IO (V.Vector (Complex Double))
generateSpreadSpectrum freqs sampleRate duration = do
    let t = [0, 1 / sampleRate .. duration - 1 / sampleRate]
    return $ V.fromList [sum [cos (2 * pi * f * ti) :+ sin (2 * pi * f * ti) | f <- freqs] | ti <- t]

-- | Generates a composite signal with multiple signatures and anomalies.
generateCompositeSignal :: Int                -- ^ Number of samples
                        -> Double             -- ^ Base frequency
                        -> Double             -- ^ Sample rate
                        -> [Signature]        -- ^ List of signatures to inject
                        -> [AnomalyPattern]   -- ^ List of anomalies to inject
                        -> Double             -- ^ Noise amplitude
                        -> IO Signal
generateCompositeSignal sampleCount baseFreq sampleRate signatures anomalies noiseAmp = do
    let baseSignal = generateBaseSignal sampleCount baseFreq sampleRate
    noise <- generateWhiteNoise sampleCount noiseAmp
    let noisySignal = addNoise (samples baseSignal) noise
    signalWithSignatures <- foldM (\sig s -> return $ injectSignature (signaturePattern s) 500 sig) noisySignal signatures
    let finalSignal = foldl (\sig a -> injectAnomaly (anomalyPattern a) (anomalyPos a) sig) signalWithSignatures anomalies
    return $ Signal sampleRate finalSignal