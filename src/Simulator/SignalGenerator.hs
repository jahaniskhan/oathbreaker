{-# LANGUAGE OverloadedStrings #-}

module Simulator.SignalGenerator
    ( generateBaseSignal
    , injectSignature
    , injectAnomaly
    , generateWhiteNoise
    , addNoise
    , generateCompositeSignal
    , generateInterferenceDeterministic
    , generateInterferenceStochastic
    , InterferenceType(..)
    , Signature(..)
    , AnomalyPattern(..)
    , Signal(..)
    , injectSignatureRandom
    , injectAnomalyRandom
    ) where

--import Prelude (cos, pi, sqrt)
import Data.Complex (Complex(..), cis)
import Control.Monad (replicateM, foldM)
import Data.List (splitAt, foldl1)
import qualified Data.Vector.Storable as V
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC
import Control.Monad.Primitive (PrimState)
import System.Random (RandomGen, randomR)

-- Define data types
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

data Signature = Signature
    { signatureName :: String
    , signaturePattern :: [Complex Double]
    }

data AnomalyPattern = AnomalyPattern
    { anomalyPattern :: [Complex Double]
    , anomalyPos :: Int
    }

data Signal = Signal
    { sampleRate :: Double
    , samples :: V.Vector (Complex Double)
    }

-- | Generates the base signal.
generateBaseSignal :: Int    -- ^ Number of samples
                   -> Double -- ^ Base frequency in Hz
                   -> Double -- ^ Base sample rate in Hz
                   -> Signal
generateBaseSignal numSamples freq sr =
    let t = [0, 1 / sr .. (fromIntegral numSamples - 1) / sr]
        baseSamples = V.fromList [cos (2 * pi * freq * ti) :+ 0 | ti <- t]
    in Signal sr baseSamples

-- | Injects a signature into the signal at a fixed position.
injectSignature :: [Complex Double] -- ^ Signature pattern
                -> Int               -- ^ Position to inject
                -> V.Vector (Complex Double) -- ^ Original signal
                -> V.Vector (Complex Double)
injectSignature signature pos signal =
    let (before, after) = V.splitAt pos signal
        signatureVector = V.fromList signature
    in before V.++ signatureVector V.++ V.drop (pos + V.length signatureVector) after

-- | Injects an anomaly into the signal.
injectAnomaly :: [Complex Double] -- ^ Anomaly pattern
              -> Int               -- ^ Position to inject
              -> V.Vector (Complex Double) -- ^ Original signal
              -> V.Vector (Complex Double)
injectAnomaly anomaly pos signal =
    let (before, after) = V.splitAt pos signal
        anomalyVector = V.fromList anomaly
    in before V.++ anomalyVector V.++ V.drop (pos + V.length anomalyVector) after

-- | Generates random white noise to be added to the signal.
generateWhiteNoise :: MWC.GenIO -> Int -> Double -> IO (V.Vector (Complex Double))
generateWhiteNoise gen numSamples noiseAmplitude = 
    V.replicateM numSamples $ do
        realPart <- MWC.normal 0 (noiseAmplitude / sqrt 2) gen
        imagPart <- MWC.normal 0 (noiseAmplitude / sqrt 2) gen
        return (realPart :+ imagPart)

-- | Combines base signal with noise.
addNoise :: V.Vector (Complex Double)    -- ^ Base signal
         -> V.Vector (Complex Double)    -- ^ Noise
         -> V.Vector (Complex Double)
addNoise = V.zipWith (+)

-- | Generates deterministic interference signal based on the specified InterferenceType.
generateInterferenceDeterministic :: InterferenceType -> Double -> Double -> V.Vector (Complex Double)
generateInterferenceDeterministic interferenceType sr duration = 
    case interferenceType of
        Tone frequency -> generateTone frequency sr duration
        Sweep startFreq endFreq -> generateSweep startFreq endFreq sr duration
        MultiTone frequencies -> generateMultiTone frequencies sr duration
        _ -> V.empty -- Handle other deterministic types or throw an error

-- | Generates stochastic interference signal based on the specified InterferenceType.
generateInterferenceStochastic :: MWC.GenIO -> InterferenceType -> Double -> Double -> IO (V.Vector (Complex Double))
generateInterferenceStochastic gen interferenceType sr duration = 
    case interferenceType of
        WhiteNoise -> generateWhiteNoise gen (floor (sr * duration)) 1.0
        QAM order symbolRate -> generateQAM gen order symbolRate sr duration
        OFDM subcarriers -> generateOFDM gen subcarriers sr duration
        Chirp startFreq endFreq -> generateChirp gen startFreq endFreq sr duration
        SpreadSpectrum freqs -> generateSpreadSpectrum gen freqs sr duration
        MultiTone frequencies -> generateMultiToneStochastic gen frequencies sr duration
        _ -> return V.empty -- Handle other stochastic types or throw an error

-- | Generates a tone interference signal.
generateTone :: Double -> Double -> Double -> V.Vector (Complex Double)
generateTone frequency sr duration =
    let numSamples = floor (sr * duration) :: Int
        t = [0, 1 / sr .. (fromIntegral numSamples - 1) / sr]
    in V.fromList [cos (2 * pi * frequency * ti) :+ 0 | ti <- t]

-- | Generates a frequency sweep interference signal.
generateSweep :: Double -> Double -> Double -> Double -> V.Vector (Complex Double)
generateSweep startFreq endFreq sr duration =
    let numSamples = floor (sr * duration) :: Int
        t = [0, 1 / sr .. (fromIntegral numSamples - 1) / sr]
        freqs = [startFreq + (endFreq - startFreq) * ti / duration | ti <- take numSamples t]
    in V.fromList [cos (2 * pi * f * ti) :+ 0 | (f, ti) <- zip freqs t]

-- | Generates a multi-tone interference signal.
generateMultiTone :: [Double] -> Double -> Double -> V.Vector (Complex Double)
generateMultiTone frequencies sr duration =
    let numSamples = floor (sr * duration) :: Int
        t = [0, 1 / sr .. (fromIntegral numSamples - 1) / sr]
    in foldl1 (V.zipWith (+)) [V.fromList [cos (2 * pi * f * ti) :+ 0 | ti <- t] | f <- frequencies]

-- | Generates a stochastic multi-tone interference signal by adding random phases to each tone.
generateMultiToneStochastic :: MWC.GenIO -> [Double] -> Double -> Double -> IO (V.Vector (Complex Double))
generateMultiToneStochastic gen frequencies sr duration = do
    let numSamples = floor (sr * duration) :: Int
        t = [0, 1 / sr .. (fromIntegral numSamples - 1) / sr]
    -- For each frequency, generate a list of samples with a random phase
    interference <- mapM (\f -> do
                             phase <- MWC.uniformR (0, 2 * pi) gen
                             return [cos (2 * pi * f * ti + phase) :+ sin (2 * pi * f * ti + phase) | ti <- t]
                         ) frequencies
    -- Sum all interference vectors element-wise
    let summedInterference = foldl1 (V.zipWith (+)) [V.fromList s | s <- interference]
    return summedInterference

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

-- | Generates a composite signal with base signal, noise, signatures, and anomalies.
generateCompositeSignal :: MWC.GenIO         -- ^ Random number generator
                       -> Int               -- ^ Number of samples
                       -> Double            -- ^ Base frequency in Hz
                       -> Double            -- ^ Base sample rate in Hz
                       -> [Signature]       -- ^ List of signatures to inject
                       -> [AnomalyPattern]  -- ^ List of anomalies to inject
                       -> Double            -- ^ Noise amplitude
                       -> IO Signal
generateCompositeSignal gen sampleCount baseFreq baseSampleRate signatures anomalies noiseAmp = do
    -- Step 1: Generate the base signal
    let baseSignal = generateBaseSignal sampleCount baseFreq baseSampleRate
    
    -- Step 2: Generate white noise
    noiseVector <- generateWhiteNoise gen sampleCount noiseAmp
    
    -- Step 3: Add noise to the base signal to create noisySignal
    let noisySignal = addNoise (samples baseSignal) noiseVector
    
    -- Step 4: Inject signatures into the noisy signal
    signalWithSignatures <- foldM (\sig s -> injectSignatureRandom gen (signaturePattern s) sig) noisySignal signatures
    
    -- Step 5: Inject anomalies into the signal with signatures
    finalSignal <- foldM (\sig a -> injectAnomalyRandom gen a sig) signalWithSignatures anomalies
    
    -- Step 6: Return the final composite signal
    return $ Signal baseSampleRate finalSignal

-- | Injects a signature at a random position within the signal.
injectSignatureRandom :: MWC.GenIO -> [Complex Double] -> V.Vector (Complex Double) -> IO (V.Vector (Complex Double))
injectSignatureRandom gen pattern signal = do
    let maxPos = V.length signal - length pattern
    pos <- MWC.uniformR (0, maxPos) gen
    return $ injectSignature pattern pos signal

-- | Injects an anomaly at a random position within the signal.
injectAnomalyRandom :: MWC.GenIO -> AnomalyPattern -> V.Vector (Complex Double) -> IO (V.Vector (Complex Double))
injectAnomalyRandom gen (AnomalyPattern pattern _) signal = do
    let maxPos = V.length signal - length pattern
    pos <- MWC.uniformR (0, maxPos) gen
    return $ injectAnomaly pattern pos signal