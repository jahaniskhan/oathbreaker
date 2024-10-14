{-# LANGUAGE OverloadedStrings #-}

module Main where

import Simulator.SignalGenerator
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
    )
import ThreatDetection.AnomalyDetection
import ThreatDetection.SignatureMatching
import ProactiveDefense.AdaptiveJamming
import RFProcessor.SpectralAnalysis
import RFProcessor.DigitalSignalProcessing
import RFProcessor.SignalAcquisition
import Common.Types
import Utils.PCA
import Utils.MathFunction
import Utils.SDRInterface
import Utils.DataStructures

import Data.Complex (Complex, magnitude)
import qualified Data.Vector.Storable as V
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Time
import System.Directory (createDirectoryIfMissing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (forM_)
import System.Random (newStdGen)
import Control.Monad.State (runState, State)
import qualified System.Random.MWC as MWC

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
        signatures = [Signature "Sig1" [1 :+ 0, 2 :+ 0, 3 :+ 0], Signature "Sig2" [4 :+ 0, 5 :+ 0, 6 :+ 0]]
        anomalies = [AnomalyPattern [7 :+ 0, 8 :+ 0, 9 :+ 0] 1000, AnomalyPattern [10 :+ 0, 11 :+ 0, 12 :+ 0] 2000]

    -- Create a new MWC generator
    gen <- MWC.createSystemRandom

    -- Generate composite signal
    compositeSignal <- generateCompositeSignal gen numSamples baseFreq sampleRate signatures anomalies noiseAmp
    let compositeSignalSamples = samples compositeSignal

    -- Generate Interference
    let interferenceType = MultiTone [200, 300] -- Example: Deterministic Interference
    interferenceSignal <- case interferenceType of
        WhiteNoise -> generateInterferenceStochastic gen interferenceType sampleRate duration
        QAM _ _ -> generateInterferenceStochastic gen interferenceType sampleRate duration
        OFDM _ -> generateInterferenceStochastic gen interferenceType sampleRate duration
        Chirp _ _ -> generateInterferenceStochastic gen interferenceType sampleRate duration
        SpreadSpectrum _ -> generateInterferenceStochastic gen interferenceType sampleRate duration
        Tone _ -> generateInterferenceDeterministic interferenceType sampleRate duration
        Sweep _ _ -> generateInterferenceDeterministic interferenceType sampleRate duration
        MultiTone _ -> generateInterferenceDeterministic interferenceType sampleRate duration

    -- Combine Signals
    let combinedSignal = Signal sampleRate $ addNoise compositeSignalSamples interferenceSignal

    -- SDR Acquisition
    let sdrParams = SDRParams sampleRate centerFreq 1.0
    acquiredSignal <- acquireSignal sdrParams combinedSignal

    -- | Calculate Power Spectral Density (PSD)
    --
    -- Analyze the acquired signal to determine its power distribution across different frequencies.
    -- PSD is a crucial metric for identifying signal strengths and potential interferences.
    let psd = calculatePowerSpectralDensity (V.toList (samples acquiredSignal))

    -- TODO: Visualize PSD to aid in the analysis of signal characteristics.

    -- | Detect Threats: Anomalies and Signature Matching
    --
    -- Utilize detection algorithms to identify anomalies and match known signatures within the acquired signal.
    -- This step is essential for threat analysis and ensuring signal integrity.
    let detectedAnomalies = detectAnomalies (V.toList (samples acquiredSignal)) 2.0 5 128 64 sampleRate
        detectedSignatures = matchSignatures signatures acquiredSignal

    -- TODO: Refine detection parameters or employ machine learning techniques for improved accuracy.

    -- | Generate Adaptive Jamming Signal
    --
    -- Based on detected anomalies, generate a jamming signal using Principal Component Analysis (PCA).
    -- The jamming signal aims to neutralize or mitigate detected threats.
    jammingSignalResult <- adaptiveJammingPCA 2 detectedAnomalies sampleRate duration

    case jammingSignalResult of
        Left err -> putStrLn $ "Error generating jamming signal: " ++ err
        Right jammingSignal -> do
            -- Logging
            let logFile = "simulations/" ++ simulationId ++ "/simulation_log.txt"
            TIO.writeFile logFile $ T.unlines
                [ "Simulation ID: " <> T.pack simulationId
                , "Sample Rate: " <> T.pack (show sampleRate) <> " Hz"
                , "Duration: " <> T.pack (show duration) <> " s"
                , "Center Frequency: " <> T.pack (show centerFreq) <> " Hz"
                , "Detected Anomalies: " <> T.pack (show (length detectedAnomalies))
                , "Detected Signatures: " <> T.pack (show detectedSignatures)
                , "Jamming Signal Length: " <> T.pack (show (LA.size jammingSignal))
                , "\nDetailed Anomalies:"
                , T.unlines $ map (T.pack . show) detectedAnomalies
                ]

            putStrLn $ "Results logged to " ++ logFile

            -- TODO: Implement a logging mechanism that categorizes logs by severity levels (e.g., INFO, WARNING, ERROR).

            -- | Visualize Signals
            --
            -- Generate visual representations of the composite, acquired, and jamming signals.
            -- These visualizations aid in the qualitative analysis of signal behaviors.
            putStrLn "Generating visualizations..."
            let plotFile = "simulations/" ++ simulationId ++ "/signal_plot.svg"
            plotSignals plotFile
                [ ("Composite Signal", V.toList compositeSignalSamples)
                , ("Acquired Signal", V.toList (samples acquiredSignal))
                , ("Jamming Signal", LA.toList jammingSignal)
                ]
            putStrLn $ "Signal plot saved to " ++ plotFile

            -- TODO: Expand visualization to include time-domain and frequency-domain plots for comprehensive analysis.

            -- | Record Signal Data
            --
            -- Save the magnitude of the composite, acquired, and jamming signals into a CSV file.
            -- This data facilitates further quantitative analysis and processing.
            let recordFile = "simulations/" ++ simulationId ++ "/signal_data.csv"
            recordSignalData recordFile compositeSignalSamples (V.toList (samples acquiredSignal)) (LA.toList jammingSignal)
            putStrLn $ "Signal data recorded to " ++ recordFile

            -- TODO: Incorporate additional signal attributes (e.g., phase information) into the recorded data for enhanced insights.

            putStrLn "Simulation complete."

-- | Function to plot signals
--
-- Generates a visualization of multiple signals and saves it as an SVG file.
-- Each signal is represented with its magnitude across sample points.
plotSignals :: FilePath -> [(String, [Complex Double])] -> IO ()
plotSignals filename signals = do
    let chart = toRenderable $ do
            layout_title .= "Signal Comparison"        -- Title of the chart
            layout_x_axis . laxis_title .= "Sample"     -- Label for the x-axis
            layout_y_axis . laxis_title .= "Magnitude"  -- Label for the y-axis
            forM_ signals $ \(name, signal) -> do
                -- Plot each signal as a separate line on the chart
                plot (line name [[(x, magnitude y) | (x, y) <- zip [0..] signal]])
    renderableToFile def filename chart

-- | Function to record signal data
--
-- Saves the magnitude of the composite, acquired, and jamming signals into a CSV file.
-- Each row in the CSV corresponds to a sample point with its respective magnitudes.
recordSignalData :: FilePath -> V.Vector (Complex Double) -> [Complex Double] -> [Double] -> IO ()
recordSignalData filename compositeSignal acquiredSignal jammingSignal = do
    let csvData = "Composite,Magnitude,Acquired,Magnitude,Jamming\n" ++
                  unlines (map (\(c, a, j) -> show (magnitude c) ++ "," ++ show (magnitude a) ++ "," ++ show j) (zip3 (V.toList compositeSignal) acquiredSignal jammingSignal))
    writeFile filename csvData
    -- TODO: Include headers in the CSV file for clarity (e.g., "Composite,Magnitude,Acquired,Magnitude,Jamming").
