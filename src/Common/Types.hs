module Common.Types (
    AcquisitionConfig(..),
    SignalSource(..),
    SimulatedSignal(..),
    AcquisitionError(..),
    Anomaly(..)
) where

-- Removed unused import of Data.Complex
-- If only instances are needed, import it as follows:
import Data.Complex ()  -- Import instances only

--import Data.IORef (IORef)

-- Define your data types here
data AcquisitionConfig = AcquisitionConfig
    { acqSource     :: SignalSource
    , acqSampleRate :: Double
    , acqDuration   :: Double
    , acqCenterFreq :: Double
    } deriving (Show, Eq)

data SignalSource
    = SDR
    | Simulated SimulatedSignal
    deriving (Show, Eq)

data SimulatedSignal
    = WhiteNoise
    | Sine Double          -- Frequency
    | MultiTone [Double]
    | QPSK Double          -- Symbol Rate
    deriving (Show, Eq)

data AcquisitionError
    = SourceNotFound
    | InvalidConfig String
    | ParsingFailed String
    | InsufficientVariance
    deriving (Show, Eq)

data Anomaly = Anomaly
    { anomalyFrequencyBin :: Int
    , anomalyMagnitude    :: Double
    , anomalyType         :: AnomalyType
    , anomalyScore        :: Double
    } deriving (Show, Eq)

data AnomalyType = UnexpectedPeak | SignalDropout | NoiseBurst | SpectrumAnomaly | TimeFrequencyAnomaly
    deriving (Show, Eq)
