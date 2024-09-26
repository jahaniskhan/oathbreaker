module Common.Types (
    AcquisitionConfig(..),
    SignalSource(..),
    SimulatedSignal(..),
    AcquisitionError(..),
    Anomaly(..),
    AnomalyType(..) -- Exposing the AnomalyType for external usage
) where

-- | Configuration settings for signal acquisition.
data AcquisitionConfig = AcquisitionConfig
    { sampleRate   :: Double -- ^ Sampling rate in Hz
    , signalLength :: Int    -- ^ Number of samples to acquire
    } deriving (Show, Eq)

-- | Represents the source of the signal.
data SignalSource = RealSignal String        -- ^ Real signal from a specified source.
                  | Simulated SimulatedSignal -- ^ Simulated signal.
    deriving (Show, Eq)

-- | Represents a simulated signal with specific parameters.
data SimulatedSignal = SimulatedSignal
    { frequency :: Double   -- ^ Frequency in Hz
    , amplitude :: Double   -- ^ Amplitude of the signal
    , noiseLevel :: Double  -- ^ Noise level to be added
    } deriving (Show, Eq)

-- | Errors that can occur during signal acquisition.
data AcquisitionError = SignalTimeout
                      | InvalidConfiguration String
                      | HardwareFailure String
    deriving (Show, Eq)

-- | Represents an anomaly detected in the signal.
data Anomaly = Anomaly
    { anomalyFrequencyBin :: Int            -- ^ Frequency bin where anomaly was detected.
    , anomalyMagnitude    :: Double         -- ^ Magnitude of the anomaly.
    , anomalyType         :: AnomalyType    -- ^ Type/category of the anomaly.
    , anomalyScore        :: Double         -- ^ Score indicating the severity or confidence of the anomaly.
    } deriving (Show, Eq)

-- | Enumerates the types of anomalies that can be detected.
data AnomalyType = UnexpectedPeak       -- ^ An unexpected peak detected in the spectrum.
                 | SignalDropout        -- ^ A dropout or drop in signal strength.
                 | NoiseBurst           -- ^ A burst of noise interfering with the signal.
                 | SpectrumAnomaly      -- ^ General anomalies in the frequency spectrum.
                 | TimeFrequencyAnomaly -- ^ Anomalies detected in the time-frequency domain.
    deriving (Show, Eq)