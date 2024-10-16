{-# LANGUAGE NamedFieldPuns #-}

module RFProcessor.SignalAcquisition
    ( acquireSignal
    , SDRParams(..)
    ) where

import Common.Types
import Simulator.SignalGenerator (Signal(..), generateWhiteNoise)
import Data.Complex (Complex(..))
import qualified Data.Vector.Storable as V
import System.Random.MWC (GenIO, uniformR)
import qualified System.Random.MWC as MWC
import Numeric.LinearAlgebra (Vector, fromList)
import qualified Numeric.LinearAlgebra as LA

-- | SDR Acquisition Parameters
data SDRParams = SDRParams
    { sdrSampleRate      :: Double
    , sdrCenterFrequency :: Double
    , sdrGain            :: Double
    } deriving (Show, Eq)


-- | Simulates SDR hardware signal acquisition
acquireSignal :: SDRParams -> Signal -> IO Signal
acquireSignal SDRParams { sdrSampleRate, centerFrequency, gain } (Signal _ samples) = do
    gen <- MWC.createSystemRandom
    -- Simulate hardware noise
    noise <- generateWhiteNoise gen (V.length samples) (gain * 0.05)  -- 5% of gain
    -- Simulate signal degradation (e.g., attenuation)
    let attenuatedSignal = V.map (* (gain * 0.8)) samples  -- 80% of gain
        acquiredSamples = V.zipWith (+) attenuatedSignal noise
    return $ Signal sampleRate acquiredSamples
