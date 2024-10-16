{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Simulator.SignalGenerator
    ( generateCompositeSignal
    , generateInterferenceDeterministic
    , generateInterferenceStochastic
    , InterferenceType(..)
    , Signature(..)
    , AnomalyPattern(..)
    )
import ThreatDetection.AnomalyDetection
    ( detectAnomalies
    , Anomaly(..)
    , AnomalyType(..)
    )
import ThreatDetection.SignatureMatching
    ( matchSignatures
    , MatchResult(..)
    )
import ProactiveDefense.AdaptiveJamming
    ( adaptiveJamming
    , adaptiveJammingPCA
    )
import RFProcessor.SpectralAnalysis
    ( calculatePowerSpectralDensity
    , detectPeaks
    )
import RFProcessor.DigitalSignalProcessing
    ( waveletTransform
    , adaptiveFilter
    , SignalMetadata(..)
    , runRFProcessor
    )
import RFProcessor.SignalAcquisition
    ( acquireSignal
    , SDRParams(..)
    )
import Common.Types
import Utils.PCA
import Utils.MathFunction

import Data.Complex (Complex, magnitude, cis)
import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as LA
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Time
import System.Directory (createDirectoryIfMissing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (forM_, unless, when, void)
import Control.Monad.State (runState, State)
import qualified System.Random.MWC as MWC
import Control.Concurrent (threadDelay)
import Control.Exception (catch, SomeException)

main :: IO ()
main = do
    -- Retrieve the current system time to generate a unique simulation identifier.
    currentTime <- getCurrentTime
    let simulationId = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" currentTime
    createDirectoryIfMissing True $ "simulations/" ++ simulationId

    putStrLn $ "Starting RF Simulation " ++ simulationId

    -- Simulation Parameters
    let sampleRate = 1000 :: Double
        duration = 1 :: Double
        centerFreq = 150 :: Double
        numSamples = floor (sampleRate * duration)
        baseFreq = 100 :: Double
        noiseAmp = 0.1 :: Double
        signatures = [ Signature "Sig1" [1 :+ 0, 2 :+ 0, 3 :+ 0]
                    , Signature "Sig2" [4 :+ 0, 5 :+ 0, 6 :+ 0]
                    ]
        anomalies = [ AnomalyPattern [7 :+ 0, 8 :+ 0, 9 :+ 0] 1000
                   , AnomalyPattern [10 :+ 0, 11 :+ 0, 12 :+ 0] 2000
                   ]

    -- Create a new MWC generator
    gen <- MWC.createSystemRandom

    -- Initialize SDR Parameters
    let sdrParams = SDRParams sampleRate centerFreq 1.0

    -- Define Signal Metadata
    let metadata = SignalMetadata
            { sampleRate = sampleRate
            , centerFrequency = centerFreq
            , gain = 1.0
            }

    -- Initialize Simulation Loop Parameters
    let simulationIterations = 100  -- Number of simulation steps
        sleepTimeMicroseconds = 100000  -- 0.1 seconds between iterations

    -- Start the Continuous Simulation Loop
    runSimulationLoop simulationId gen numSamples baseFreq noiseAmp signatures anomalies sdrParams metadata simulationIterations sleepTimeMicroseconds

    putStrLn "RF Simulation terminated."

-- | Runs the continuous simulation loop
runSimulationLoop :: String               -- ^ Simulation ID
                 -> MWC.GenIO            -- ^ Random generator
                 -> Int                  -- ^ Number of samples
                 -> Double               -- ^ Base frequency
                 -> Double               -- ^ Noise amplitude
                 -> [Signature]          -- ^ List of signatures
                 -> [AnomalyPattern]     -- ^ List of anomalies
                 -> SDRParams            -- ^ SDR parameters
                 -> SignalMetadata       -- ^ Signal metadata
                 -> Int                  -- ^ Number of iterations
                 -> Int                  -- ^ Sleep time in microseconds
                 -> IO ()
runSimulationLoop simulationId gen numSamples baseFreq noiseAmp signatures anomalies sdrParams metadata 0 _ = putStrLn "Completed all simulation iterations."
runSimulationLoop simulationId gen numSamples baseFreq noiseAmp signatures anomalies sdrParams metadata iter sleepTime = do
    -- 1. Signal Generation
    compositeSignal <- generateCompositeSignal gen numSamples baseFreq (sampleRate metadata) signatures anomalies noiseAmp
    let compositeSignalSamples = samples compositeSignal

    -- 2. SDR Acquisition
    acquiredSignal <- acquireSignal sdrParams compositeSignal

    -- 3. Digital Signal Processing
    -- Apply Wavelet Transformation
    waveletResult <- runRFProcessor (waveletTransform Haar 2) acquiredSignal metadata
    let (waveletTransformed, _) = waveletResult

    -- Apply Adaptive Filtering
    -- Use the wavelet-transformed signal as desired for filtering
    filteredSignalResult <- runRFProcessor (adaptiveFilter (samples acquiredSignal) 0.01 5) acquiredSignal metadata
    let (filteredSignal, _) = filteredSignalResult

    -- 4. Spectral Analysis
    let psd = calculatePowerSpectralDensity (V.toList (samples acquiredSignal))
    let detectedPeaks = detectPeaks psd 5  -- Window size for peak detection

    -- 5. Anomaly Detection
    let detectedAnomalies = detectAnomalies (V.toList (samples acquiredSignal)) 2.0 5 128 64 (sampleRate metadata)

    -- 6. Signature Matching
    let detectedSignatures = matchSignatures signatures (V.toList (samples acquiredSignal))

    -- 7. Anomaly Validation
    let validatedAnomalies = validateAnomalies detectedAnomalies

    -- 8. Adaptive Jamming
    jammingSignalResult <- adaptiveJammingPCA 2 validatedAnomalies (sampleRate metadata) duration

    -- 9. Final Signal Processing
    case jammingSignalResult of
        Left err -> putStrLn $ "Error generating jamming signal: " ++ err
        Right jammingSignal -> do
            let finalSignal = addNoise (V.toList (samples acquiredSignal)) (LA.toList jammingSignal)

            -- 10. Logging
            appendLog simulationId iter detectedAnomalies detectedSignatures jammingSignal

            -- 11. Visualization and Recording
            visualizeAndRecord simulationId iter compositeSignalSamples acquiredSignal jammingSignal

    -- 12. Sleep and Continue
    threadDelay sleepTime
    runSimulationLoop simulationId gen numSamples baseFreq noiseAmp signatures anomalies sdrParams metadata (iter - 1) sleepTime

  where
    duration = 1.0 :: Double  -- Duration remains constant for simplicity

    -- | Validates anomalies before jamming
    validateAnomalies :: [Anomaly] -> [Anomaly]
    validateAnomalies = filter isSignificant
      where
        isSignificant :: Anomaly -> Bool
        isSignificant Anomaly { anomalyScore, anomalyMagnitude } =
            anomalyScore > 1.0 && anomalyMagnitude > 0.5  -- Example thresholds

    -- | Appends simulation data to a log file
    appendLog :: String -> Int -> [Anomaly] -> [MatchResult] -> [Double] -> IO ()
    appendLog simulationId iter detectedAnomalies detectedSignatures jammingSignal = do
        let logFile = "simulations/" ++ simulationId ++ "/simulation_log.txt"
        TIO.appendFile logFile $ T.unlines
            [ "Iteration: " <> T.pack (show (100 - iter + 1))
            , "Detected Anomalies: " <> T.pack (show (length detectedAnomalies))
            , "Validated Anomalies: " <> T.pack (show (length (filter isValid detectedAnomalies)))
            , "Detected Signatures: " <> T.pack (show (length detectedSignatures))
            , "Jamming Signal Length: " <> T.pack (show (length jammingSignal))
            , "\nDetailed Anomalies:"
            , T.unlines $ map (T.pack . show) (filter isValid detectedAnomalies)
            , "-----------------------------"
            ]
      where
        isValid :: Anomaly -> Bool
        isValid Anomaly { anomalyScore, anomalyMagnitude } =
            anomalyScore > 1.0 && anomalyMagnitude > 0.5

    -- | Function to plot signals
    --
    -- Generates a visualization of multiple signals and saves it as an SVG file.
    -- Each signal is represented with its magnitude across sample points.
    visualizeAndRecord :: String -> Int -> V.Vector (Complex Double) -> Signal -> [Double] -> IO ()
    visualizeAndRecord simulationId iter compositeSamples acquiredSignal jammingSignal = do
        let plotFile = "simulations/" ++ simulationId ++ "/signal_plot_" ++ show iter ++ ".svg"
        plotSignals plotFile
            [ ("Composite Signal", V.toList compositeSamples)
            , ("Acquired Signal", V.toList (samples acquiredSignal))
            , ("Jamming Signal", jammingSignal)
            ]
        putStrLn $ "Iteration " ++ show (100 - iter + 1) ++ " processed and plotted."

        -- Record Signal Data
        let recordFile = "simulations/" ++ simulationId ++ "/signal_data.csv"
        recordSignalData recordFile compositeSamples (samples acquiredSignal) jammingSignal
        putStrLn $ "Signal data recorded to " ++ recordFile

    -- | Function to plot signals
    plotSignals :: FilePath -> [(String, [Complex Double])] -> IO ()
    plotSignals filename signals = do
        let chart = toRenderable $ do
                layout_title .= "Signal Comparison"
                layout_x_axis . laxis_title .= "Sample"
                layout_y_axis . laxis_title .= "Magnitude"
                forM_ signals $ \(name, signal) -> do
                    plot (line name [[(x, magnitude y) | (x, y) <- zip [0..] signal]])
        renderableToFile def filename chart

    -- | Function to record signal data
    --
    -- Saves the magnitude of the composite, acquired, and jamming signals into a CSV file.
    -- Each row in the CSV corresponds to a sample point with its respective magnitudes.
    recordSignalData :: FilePath -> V.Vector (Complex Double) -> V.Vector (Complex Double) -> [Double] -> IO ()
    recordSignalData filename compositeSignal acquiredSignal jammingSignal = do
        let compositeList = V.toList compositeSignal
            acquiredList = V.toList acquiredSignal
            jammingList = jammingSignal
            maxLen = maximum [length compositeList, length acquiredList, length jammingList]
            compositePadded = take maxLen $ compositeList ++ repeat (0 :+ 0)
            acquiredPadded = take maxLen $ acquiredList ++ repeat (0 :+ 0)
            jammingPadded = take maxLen $ jammingList ++ repeat 0.0
            csvData = if null compositeList
                        then "Composite,Magnitude,Acquired,Magnitude,Jamming\n"
                        else ""
                      ++ unlines (map formatRow $ zip3 compositePadded acquiredPadded jammingPadded)
            formatRow (c, a, j) = show (magnitude c) ++ "," ++ show (magnitude a) ++ "," ++ show j
        when (null compositeList) $ writeFile filename "Composite,Magnitude,Acquired,Magnitude,Jamming\n"  -- Write headers once
        appendFile filename csvData
