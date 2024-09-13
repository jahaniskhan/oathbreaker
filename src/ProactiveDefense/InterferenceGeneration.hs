module ProactiveDefense.InterferenceGeneration
    ( generateInterference
    , InterferenceType(..)
    ) where

import Data.Complex
import System.Random.MWC (Gen, createSystemRandom, uniformR)
import System.Random.MWC.Distributions (normal)
import Control.Monad (replicateM)


-- | Types of interference that can be generated
data InterferenceType
    = WhiteNoise
    | Tone Double  -- ^ Single frequency tone
    | Sweep Double Double  -- ^ Frequency sweep from start to end frequency
    | QAM Int Double  -- ^ QAM with specified order and symbol rate
    | OFDM [Double]   -- ^ OFDM with specified subcarrier frequencies
    | Chirp Double Double  -- ^ Chirp signal from start to end frequency
    deriving (Show)

-- | Generate an interference signal
generateInterference :: InterferenceType -> Double -> Double -> IO [Complex Double]
generateInterference WhiteNoise sampleRate duration = do
    let numSamples = floor (sampleRate * duration)
    whiteNoise <- generateWhiteNoise numSamples
    return $ map (:+ 0) whiteNoise

generateInterference (Tone frequency) sampleRate duration =
    let numSamples = floor (sampleRate * duration)
        t = [0, 1 / sampleRate .. duration - 1 / sampleRate]
    in return [cos (2 * pi * frequency * ti) :+ sin (2 * pi * frequency * ti) | ti <- t]

generateInterference (Sweep startFreq endFreq) sampleRate duration =
    let numSamples = floor (sampleRate * duration)
        t = [0, 1 / sampleRate .. duration - 1 / sampleRate]
        freqs = [startFreq + (endFreq - startFreq) * ti / duration | ti <- t]
    in return [cos (2 * pi * f * ti) :+ sin (2 * pi * f * ti) | (f, ti) <- zip freqs t]

generateInterference (QAM order symbolRate) sampleRate duration = 
    generateQAM order symbolRate sampleRate duration

generateInterference (OFDM subcarriers) sampleRate duration = 
    generateOFDM subcarriers sampleRate duration

generateInterference (Chirp startFreq endFreq) sampleRate duration = 
    generateChirp startFreq endFreq sampleRate duration

-- | Generate white noise using a normal distribution
generateWhiteNoise :: Int -> IO [Double]
generateWhiteNoise numSamples = do
    gen <- createSystemRandom
    replicateM numSamples (normal 0 1 gen)

-- | Generate QAM signal
generateQAM :: Int -> Double -> Double -> Double -> IO [Complex Double]
generateQAM order symbolRate sampleRate duration = do
    gen <- createSystemRandom
    let numSymbols = floor (duration * symbolRate)
        symbolValues = [x :+ y | x <- [-1, 1], y <- [-1, 1]]  -- 4-QAM for simplicity
    symbols <- replicateM numSymbols (uniformR (0, length symbolValues - 1) gen)
    let qamSignal = [symbolValues !! s | s <- symbols]
        interpolatedSignal = concat $ replicate (floor (sampleRate / symbolRate)) qamSignal
    return $ take (floor (sampleRate * duration)) interpolatedSignal

-- | Generate OFDM signal
generateOFDM :: [Double] -> Double -> Double -> IO [Complex Double]
generateOFDM subcarriers sampleRate duration = do
    let t = [0, 1 / sampleRate .. duration - 1 / sampleRate]
    return [sum [cos (2 * pi * f * ti) :+ sin (2 * pi * f * ti) | f <- subcarriers] | ti <- t]

-- | Generate Chirp signal
generateChirp :: Double -> Double -> Double -> Double -> IO [Complex Double]
generateChirp startFreq endFreq sampleRate duration = do
    let t = [0, 1 / sampleRate .. duration - 1 / sampleRate]
        k = (endFreq - startFreq) / duration
    return [cos (2 * pi * (startFreq * ti + k * ti * ti / 2)) :+ sin (2 * pi * (startFreq * ti + k * ti * ti / 2)) | ti <- t]
