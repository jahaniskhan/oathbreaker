module Common.Types
    ( AcquisitionConfig(..)
    , AcquisitionError(..)
    , SignalSource(..)
    , SimulatedSignalType(..)
    ) where

import Data.Complex

data SimulatedSignalType
    = WhiteNoise
    | Sine Double
    | MultiTone [Double]
    | QPSK Double
    deriving (Show)

data SignalSource
    = FileSource FilePath
    | SDRSource String
    | SimulatedSource SimulatedSignalType
    deriving (Show)

data AcquisitionConfig = AcquisitionConfig
    { acqSource :: SignalSource
    , acqSampleRate :: Double
    , acqCenterFreq :: Double
    , acqGain :: Double
    , acqNumSamples :: Int
    } deriving (Show)

data AcquisitionError
    = SourceNotFound
    | InvalidConfig String
    | HardwareError String
    deriving (Show)
