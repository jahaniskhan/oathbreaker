module Main where

import Test.Hspec
import ProactiveDefense.InterferenceGeneration
import ThreatDetection.AnomalyDetection
import ThreatDetection.SignatureMatching
import RFProcessor.DigitalSignalProcessing
import ProactiveDefense.AdaptiveJamming
import Data.Complex
import Numeric.Wavelet

main :: IO ()
main = hspec $ do
    describe "Interference Generation" $ do
        it "should generate white noise with mean 0 and standard deviation 1" $ do
            let sampleRate = 44100
                duration = 1.0
            whiteNoise <- generateInterference WhiteNoise sampleRate duration
            let realParts = map realPart whiteNoise
                mean = sum realParts / fromIntegral (length realParts)
                variance = sum (map (\x -> (x - mean) ^ 2) realParts) / fromIntegral (length realParts)
                stddev = sqrt variance
            mean `shouldSatisfy` (\x -> abs x < 0.1)
            stddev `shouldSatisfy` (\x -> abs (x - 1) < 0.1)

        it "should generate QAM signal" $ do
            let sampleRate = 44100
                duration = 1.0
                order = 4
                symbolRate = 1000
            qamSignal <- generateInterference (QAM order symbolRate) sampleRate duration
            length qamSignal `shouldBe` floor (sampleRate * duration)

        it "should generate OFDM signal" $ do
            let sampleRate = 44100
                duration = 1.0
                subcarriers = [1000, 2000, 3000]
            ofdmSignal <- generateInterference (OFDM subcarriers) sampleRate duration
            length ofdmSignal `shouldBe` floor (sampleRate * duration)

        it "should generate Chirp signal" $ do
            let sampleRate = 44100
                duration = 1.0
                startFreq = 1000
                endFreq = 2000
            chirpSignal <- generateInterference (Chirp startFreq endFreq) sampleRate duration
            length chirpSignal `shouldBe` floor (sampleRate * duration)

    describe "Anomaly Detection" $ do
        it "should detect and score various types of anomalies" $ do
            let signal = [1 :+ 0, 2 :+ 0, 3 :+ 0, 10 :+ 0, 3 :+ 0, 2 :+ 0, 1 :+ 0] ++ replicate 100 (1 :+ 0)
                baseThreshold = 2.0
                scoreThreshold = 2
                windowSize = 5
                anomalies = detectAnomalies signal baseThreshold scoreThreshold windowSize
            length anomalies `shouldSatisfy` (> 0)
            all (\a -> anomalyScore a > fromIntegral scoreThreshold) anomalies `shouldBe` True
            any (\a -> anomalyType a == SpectrumAnomaly) anomalies `shouldBe` True
            any (\a -> anomalyType a == TimeFrequencyAnomaly) anomalies `shouldBe` True

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
