module Main where

import Test.Hspec
import ProactiveDefense.InterferenceGeneration
import ThreatDetection.AnomalyDetection
import ThreatDetection.SignatureMatching
import RFProcessor.DigitalSignalProcessing
import ProactiveDefense.AdaptiveJamming
import RFProcessor.SpectralAnalysis
import RFProcessor.SignalAcquisition
import Utils.PCA
import Utils.MathFunction
import Utils.DataStructures
import Utils.Wavelet
import Simulator.SignalGenerator
import Common.Types
import Data.Complex
import Numeric.LinearAlgebra (Matrix, Vector)
import qualified Numeric.LinearAlgebra as LA
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = hspec $ do
    describe "Interference Generation" $ do
        it "should generate white noise with mean 0 and standard deviation 1" $ do
            let sampleRate = 44100
                duration = 1.0
            whiteNoise <- generateInterference WhiteNoise sampleRate duration
            let realParts = map realPart whiteNoise
                meanVal = sum realParts / fromIntegral (length realParts)
                variance = sum (map (\x -> (x - meanVal) ** 2) realParts) / fromIntegral (length realParts)
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

    describe "Signature Matching" $ do
        it "should match signature using DTW" $ do
            let signal = [1 :+ 0, 2 :+ 0, 3 :+ 0, 4 :+ 0]
                signature = Signature "Test" [1 :+ 0, 2 :+ 0, 3 :+ 0]
            let result = matchSignatureDTW signal signature
            matchPosition result `shouldSatisfy` (>= 0)
            matchDistance result `shouldSatisfy` (> 0)

        it "should return correct match results for multiple signatures" $ do
            let signal = [1 :+ 0, 2 :+ 0, 3 :+ 0, 4 :+ 0, 1 :+ 0, 2 :+ 0, 3 :+ 0]
                signature1 = Signature "Test1" [1 :+ 0, 2 :+ 0, 3 :+ 0]
                signature2 = Signature "Test2" [2 :+ 0, 3 :+ 0, 4 :+ 0]
                signatures = [signature1, signature2]
            let results = matchSignatures signatures signal
            length results `shouldBe` 2
            head results `shouldSatisfy` (\res -> matchedSignature res == signature1 && matchPosition res == 0)
            results !! 1 `shouldSatisfy` (\res -> matchedSignature res == signature2 && matchPosition res == 1)

    describe "Adaptive Jamming" $ do
        it "should generate adaptive jamming signal using PCA" $ do
            let anomalies = [Anomaly 100 2.5 UnexpectedPeak, Anomaly 200 3.0 NoiseBurst]
                sampleRate = 1000
                duration = 0.1
            result <- adaptiveJammingPCA 2 anomalies sampleRate duration
            case result of
                Left err -> expectationFailure err
                Right jammingSignal -> length jammingSignal `shouldBe` floor (sampleRate * duration)

    describe "PCA Module" $ do
        it "performs PCA on a simple dataset correctly" $ do
            let dataMatrix = LA.fromLists 
                    [ [2.5, 2.4]
                    , [0.5, 0.7]
                    , [2.2, 2.9]
                    , [1.9, 2.2]
                    , [3.1, 3.0]
                    , [2.3, 2.7]
                    , [2.0, 1.6]
                    , [1.0, 1.1]
                    , [1.5, 1.6]
                    , [1.1, 0.9]
                    ]
                numComponents = 2
                components = performPCA dataMatrix numComponents
            case components of
                Nothing -> expectationFailure "PCA failed on valid data."
                Just (PrincipalComponents{ eigenvalues, eigenvectors }) -> do
                    eigenvalues `shouldSatisfy` (\vals -> length vals == 2 && all (> 0) vals)
                    LA.rows eigenvectors `shouldBe` 2
                    LA.cols eigenvectors `shouldBe` 2

        it "returns Nothing for empty data matrix" $ do
            let dataMatrix = LA.fromLists []
                numComponents = 2
            performPCA dataMatrix numComponents `shouldBe` Nothing

        it "handles requesting more components than available" $ do
            let dataMatrix = LA.fromLists 
                    [ [1.0, 2.0]
                    , [3.0, 4.0]
                    ]
                numComponents = 5
            let components = performPCA dataMatrix numComponents
            case components of
                Nothing -> expectationFailure "PCA should handle more components gracefully."
                Just (PrincipalComponents{ eigenvalues, eigenvectors }) -> do
                    LA.size eigenvalues `shouldBe` 2
                    LA.rows eigenvectors `shouldBe` 2
                    LA.cols eigenvectors `shouldBe` 2

    describe "MathFunction Module" $ do
        it "calculates median correctly for odd number of elements" $ do
            median [3, 1, 4, 1, 5] `shouldBe` Just 3

        it "calculates median correctly for even number of elements" $ do
            median [3, 1, 4, 2] `shouldBe` Just 2.5

        it "returns Nothing for empty list" $ do
            median ([] :: [Double]) `shouldBe` Nothing

        it "calculates meanVector correctly" $ do
            let mat = LA.fromLists [[1, 2], [3, 4], [5, 6]]
            meanVector mat `shouldBe` LA.fromList [3.0, 4.0]

        it "calculates stdVector correctly" $ do
            let mat = LA.fromLists [[2, 2], [4, 4], [4, 4], [4, 4], [5, 5], [5, 5], [7, 7], [9, 9]]
            stdVector mat `shouldBe` LA.fromList [2.0, 2.0]

    describe "DataStructures Module" $ do
        it "creates a new circular buffer correctly" $ do
            cb <- newCircularBuffer 3
            contents <- getBufferContents cb
            contents `shouldBe` ([] :: [Int])

        it "adds elements to the circular buffer correctly" $ do
            cb <- newCircularBuffer 3
            addToBuffer cb 1
            addToBuffer cb 2
            addToBuffer cb 3
            contents <- getBufferContents cb
            contents `shouldBe` [3,2,1]

        it "overwrites old elements when buffer exceeds its size" $ do
            cb <- newCircularBuffer 3
            addToBuffer cb 1
            addToBuffer cb 2
            addToBuffer cb 3
            addToBuffer cb 4
            contents <- getBufferContents cb
            contents `shouldBe` [4,3,2]

    describe "Wavelet Module" $ do
        it "retrieves Haar wavelet coefficients correctly" $ do
            getWaveletCoefficients Haar `shouldBe` Right [1 / sqrt 2, 1 / sqrt 2]

        it "fails for unsupported wavelet types" $ do
            getWaveletCoefficients (Daubechies 3) `shouldBe` Left (UnsupportedWavelet (Daubechies 3))

        it "retrieves wavelet filter pair for Haar correctly" $ do
            getWaveletFilterPair Haar `shouldBe` Right ([1 / sqrt 2, 1 / sqrt 2], [1 / sqrt 2, -1 / sqrt 2])

    describe "DigitalSignalProcessing Module" $ do
        it "frequencyModulate correctly modulates a signal" $ do
            let carrierFreq = 1000
                signal = [1 :+ 0, 1 :+ 0]
                metadata = SignalMetadata 1000 0 1
            (modulatedSignal, _) <- runRFProcessor (frequencyModulate carrierFreq) signal metadata
            magnitude (modulatedSignal !! 0) `shouldBe` 1.0
            magnitude (modulatedSignal !! 1) `shouldBe` 1.0

    describe "SignalAcquisition Module" $ do
        it "performSignalPCA correctly transforms data" $ do
            let dataMatrix = LA.fromLists 
                    [ [2.5, 2.4]
                    , [0.5, 0.7]
                    , [2.2, 2.9]
                    , [1.9, 2.2]
                    , [3.1, 3.0]
                    , [2.3, 2.7]
                    , [2.0, 1.6]
                    , [1.0, 1.1]
                    , [1.5, 1.6]
                    , [1.1, 0.9]
                    ]
                numComponents = 2
            let result = performSignalPCA dataMatrix numComponents
            case result of
                Left err -> expectationFailure err
                Right (transformedData, eigenvalues) -> do
                    LA.rows transformedData `shouldBe` LA.rows dataMatrix
                    LA.cols transformedData `shouldBe` numComponents
                    LA.size eigenvalues `shouldBe` numComponents

        it "sortIndices correctly sorts indices based on vector values" $ do
            let vec = LA.fromList [0.3, 0.1, 0.4, 0.2]
            sortIndices vec `shouldBe` [2,0,3,1]
