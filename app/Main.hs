{-# LANGUAGE OverloadedStrings #-}

module Main where

import Simulator.SignalGenerator
import ThreatDetection.AnomalyDetection
import ThreatDetection.SignatureMatching
import ProactiveDefense.AdaptiveJamming
import ProactiveDefense.InterferenceGeneration
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

main :: IO ()
main = do
    currentTime <- getCurrentTime
    let simulationId = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" currentTime
    createDirectoryIfMissing True $ "simulations/" ++ simulationId

    putStrLn $ "Starting RF Simulation " ++ simulationId

    -- Simulation parameters
    let sampleRate = 1000 :: Double
        duration = 1 :: Double
        centerFreq = 150 :: Double
        numSamples = floor (sampleRate * duration)
        baseFreq = 100 :: Double
        noiseAmp = 0.1 :: Double
        signatures = [Signature "Sig1" [1, 2, 3], Signature "Sig2" [4, 5, 6]]
        anomalies = [AnomalyPattern [7, 8, 9] 1000, AnomalyPattern [10, 11, 12] 2000]

    -- Generate base signal
    let baseSignal = generateBaseSignal numSamples baseFreq sampleRate

    -- Inject signatures
    let signalWithSignatures = injectSignatures baseSignal signatures

    -- Add anomalies
    let signalWithAnomalies = injectAnomalies signalWithSignatures anomalies

    -- Generate noise and add to the signal
    noise <- generateWhiteNoise numSamples noiseAmp
    let signalWithNoise = addNoise signalWithAnomalies noise

    -- Create interference and generate composite signal
    interference <- generateInterference (MultiTone [200, 300]) sampleRate duration
    let compositeSignal = generateCompositeSignal signalWithNoise interference

    -- Acquire the composite signal
    let sdrParams = SDRParams sampleRate centerFreq 1.0
    acquiredSignal <- acquireSignal sdrParams

    -- Calculate PSD
    let psd = calculatePowerSpectralDensity (V.toList (samples acquiredSignal))

    -- Detect threats
    let detectedAnomalies = detectAnomalies (V.toList (samples acquiredSignal)) 2.0 5 128 64 sampleRate
        detectedSignatures = matchSignatures signatures acquiredSignal

    -- Generate jamming signal
    jammingSignal <- adaptiveJammingPCA 2 detectedAnomalies sampleRate duration

    case jammingSignal of
        Left err -> putStrLn $ "Error generating jamming signal: " ++ err
        Right jamSignal -> do
            -- Log results
            let logFile = "simulations/" ++ simulationId ++ "/simulation_log.txt"
            TIO.writeFile logFile $ T.unlines
                [ "Simulation ID: " <> T.pack simulationId
                , "Sample Rate: " <> T.pack (show sampleRate) <> " Hz"
                , "Duration: " <> T.pack (show duration) <> " s"
                , "Center Frequency: " <> T.pack (show centerFreq) <> " Hz"
                , "Detected Anomalies: " <> T.pack (show (length detectedAnomalies))
                , "Detected Signatures: " <> T.pack (show detectedSignatures)
                , "Jamming Signal Length: " <> T.pack (show (V.length jamSignal))
                , "\nDetailed Anomalies:"
                , T.unlines $ map (T.pack . show) detectedAnomalies
                ]

            putStrLn $ "Results logged to " ++ logFile

            -- Visualize signals
            putStrLn "Generating visualizations..."
            let plotFile = "simulations/" ++ simulationId ++ "/signal_plot.svg"
            plotSignals plotFile [ ("Composite Signal", V.toList (samples compositeSignal))
                                 , ("Acquired Signal", V.toList (samples acquiredSignal))
                                 , ("Jamming Signal", V.toList jamSignal)
                                 ]
            putStrLn $ "Signal plot saved to " ++ plotFile

            -- Record data
            let recordFile = "simulations/" ++ simulationId ++ "/signal_data.csv"
            recordSignalData recordFile (V.toList (samples compositeSignal)) (V.toList (samples acquiredSignal)) (V.toList jamSignal)
            putStrLn $ "Signal data recorded to " ++ recordFile

            putStrLn "Simulation complete."

-- Function to plot signals
plotSignals :: FilePath -> [(String, [Complex Double])] -> IO ()
plotSignals filename signals = do
    let chart = toRenderable $ do
            layout_title .= "Signal Comparison"
            layout_x_axis . laxis_title .= "Sample"
            layout_y_axis . laxis_title .= "Magnitude"
            forM_ signals $ \(name, signal) -> do
                plot (line name [[(x, magnitude y) | (x, y) <- zip [0..] signal]])
    renderableToFile def filename chart

-- Function to record signal data
recordSignalData :: FilePath -> [Complex Double] -> [Complex Double] -> [Double] -> IO ()
recordSignalData filename compositeSignal acquiredSignal jammingSignal = do
    let csvData = unlines $ map (\(c, a, j) -> show (magnitude c) ++ "," ++ show (magnitude a) ++ "," ++ show j) (zip3 compositeSignal acquiredSignal jammingSignal)
    writeFile filename csvData