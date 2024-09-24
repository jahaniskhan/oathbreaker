{-# LANGUAGE OverloadedStrings #-}

module Main where

import ProactiveDefense.AdaptiveJamming
import ThreatDetection.AnomalyDetection
import RFProcessor.DigitalSignalProcessing
import RFProcessor.SignalAcquisition
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

    -- Signal acquisition
    let acquisitionConfig = AcquisitionConfig 
            { acqSource = Simulated (MultiTone [100, 200])
            , acqSampleRate = sampleRate
            , acqDuration = duration
            , acqCenterFreq = centerFreq
            }

    signalResult <- acquireSignal acquisitionConfig
    case signalResult of
        Left err -> do
            putStrLn $ "Error acquiring signal: " ++ show err
            -- logError $ "Signal acquisition failed: " ++ show err
        Right acquiredSignal -> do
            putStrLn "Signal acquired successfully"

            -- Process the signal
            putStrLn "Processing signal..."
            let metadata = SignalMetadata sampleRate centerFreq 1
            (processedSignal, _) <- runRFProcessor (do
                filtered <- applyFilter (lowPassFilter 150)
                transformed <- waveletTransform 2
                return transformed) acquiredSignal metadata

            -- Detect anomalies
            putStrLn "Detecting anomalies..."
            let baseThreshold = 2.0
                scoreThreshold = 5
                windowSize = 128
                hopSize = 64
                anomalies = detectAnomalies (V.toList processedSignal) baseThreshold scoreThreshold windowSize hopSize sampleRate

            -- Generate jamming signal
            putStrLn "Generating adaptive jamming signal..."
            jammingSignalResult <- adaptiveJammingPCA 2 anomalies sampleRate duration
            case jammingSignalResult of
                Left err -> putStrLn $ "Error generating jamming signal: " ++ err
                Right jammingSignal -> do
                    -- Log results
                    let logFile = "simulations/" ++ simulationId ++ "/simulation_log.txt"
                    TIO.writeFile logFile $ T.unlines
                        [ "Simulation ID: " <> T.pack simulationId
                        , "Sample Rate: " <> T.pack (show sampleRate) <> " Hz"
                        , "Duration: " <> T.pack (show duration) <> " s"
                        , "Center Frequency: " <> T.pack (show centerFreq) <> " Hz"
                        , "Detected Anomalies: " <> T.pack (show (length anomalies))
                        , "Jamming Signal Length: " <> T.pack (show (length jammingSignal))
                        , "\nDetailed Anomalies:"
                        , T.unlines $ map (T.pack . show) anomalies
                        ]

                    putStrLn $ "Results logged to " ++ logFile

                    -- Visualize signals
                    putStrLn "Generating visualizations..."
                    let plotFile = "simulations/" ++ simulationId ++ "/signal_plot.svg"
                    plotSignals plotFile [ ("Acquired Signal", V.toList acquiredSignal)
                                         , ("Processed Signal", V.toList processedSignal)
                                         , ("Jamming Signal", jammingSignal)
                                         ]
                    putStrLn $ "Signal plot saved to " ++ plotFile
                    putStrLn "Simulation complete."

-- Helper function for low-pass filter
lowPassFilter :: Double -> Double -> Double
lowPassFilter cutoff f = if abs f < cutoff then 1 else 0

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
