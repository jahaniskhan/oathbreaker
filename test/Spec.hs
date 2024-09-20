module Main where

import Test.Hspec
import ProactiveDefense.InterferenceGeneration
import ThreatDetection.AnomalyDetection
import ThreatDetection.SignatureMatching
import RFProcessor.DigitalSignalProcessing
import ProactiveDefense.AdaptiveJamming
import Data.Complex
import Numeric.Wavelet
import Utils.PCA (performPCA, PrincipalComponents(..))
import Numeric.LinearAlgebra (Matrix, fromLists, (<>))
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "Interference Generation" $ do
        it "should generate white noise with mean 0 and standard deviation 1" $ do
            let sampleRate = 44100
                duration = 1.0
            whiteNoise <- generateInterference WhiteNoise sampleRate duration
            let realParts = map realPart whiteNoise
                meanVal = sum realParts / fromIntegral (length realParts)
                variance = sum (map ((^2) . (meanVal -)) realParts) / fromIntegral (length realParts)
            meanVal `shouldSatisfy` (\m -> abs m < 0.1)
            variance `shouldSatisfy` (\v -> abs (v - 1.0) < 0.1)

    describe "Anomaly Detection" $ do
        it "should detect no anomalies in an identical signal" $ do
            let identicalSignal = replicate 100 1.0
                baseThreshold = 2.0
                scoreThreshold = 2
                windowSize = 5
            detectAnomalies identicalSignal baseThreshold scoreThreshold windowSize `shouldBe` []

    describe "Advanced Signal Processing" $ do
        it "should perform wavelet transform" $ do
            let signal = [1 :+ 0, 2 :+ 0, 3 :+ 0, 4 :+ 0]
                metadata = SignalMetadata 1000 0 1
            (transformedSignal, _) <- runRFProcessor (waveletTransform Haar 1) signal metadata
            length transformedSignal `shouldBe` length signal

        it "should perform adaptive filtering" $ do
            let signal = [1 :+ 0, 2 :+ 0, 3 :+ 0, 4 :+ 0]
                metadata = SignalMetadata 1000 0 1
            (filteredSignal, _) <- runRFProcessor (adaptiveFilter 2 0.1) signal metadata
            length filteredSignal `shouldBe` length signal

    describe "Enhanced Signature Matching" $ do
        it "should match signature using DTW" $ do
            let signal = [1 :+ 0, 2 :+ 0, 3 :+ 0, 4 :+ 0]
                signature = Signature "Test" [1 :+ 0, 2 :+ 0, 3 :+ 0]
            let result = matchSignatureDTW signal signature
            matchPosition result `shouldSatisfy` (>= 0)

    describe "Adaptive Jamming" $ do
        it "should generate adaptive jamming signal using PCA" $ do
            let anomalies = [Anomaly 1 2 UnexpectedPeak, Anomaly 2 3 NoiseBurst]
                sampleRate = 1000
                duration = 0.1
            jammingSignal <- adaptiveJammingPCA anomalies sampleRate duration
            length jammingSignal `shouldBe` floor (sampleRate * duration)

    describe "PCA Module" $ do
        it "performs PCA on a simple dataset correctly" $ do
            let dataMatrix = fromLists 
                    [ [2.5, 2.4]
                    , [0.5, 0.7]
                    , [2.2, 2.9]
                    , [1.9, 2.2]
                    , [3.1, 3.0]
                    , [2.3, 2.7]
                    , [2, 1.6]
                    , [1, 1.1]
                    , [1.5, 1.6]
                    , [1.1, 0.9]
                    ]
                numComponents = 2
                PrincipalComponents { eigenvalues, eigenvectors } = performPCA dataMatrix numComponents
            eigenvalues `shouldSatisfy` (\vals -> length vals == 2 && all (> 0) vals)
            length eigenvectors `shouldBe` 2

        it "returns Nothing for empty data matrix" $ do
            let dataMatrix = fromLists []
                numComponents = 2
            performPCA dataMatrix numComponents `shouldBe` Nothing

        it "handles requesting more components than available" $ do
            let dataMatrix = fromLists 
                    [ [1.0, 2.0]
                    , [3.0, 4.0]
                    ]
                numComponents = 5
                PrincipalComponents { eigenvalues, eigenvectors } = performPCA dataMatrix 5
            length eigenvalues `shouldBe` 2
            length eigenvectors `shouldBe` 2
