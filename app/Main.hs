module Main where

import RFProcessor.DigitalSignalProcessing
import Data.Complex

main :: IO ()
main = do
    -- Define the initial signal as a list of complex numbers
    let initialSignal = [1 :+ 0, 2 :+ 1, 3 :+ (-1), 4 :+ 2] :: Signal
    -- Define the initial metadata for the signal
    let initialMetadata = SignalMetadata 
        { sampleRate = 44100 -- Sample rate in Hz
        , centerFrequency = 100e6 -- Center frequency in Hz
        , gain = 1.0 -- Signal gain
        }
    
    (processedSignal, finalMetadata) <- runRFProcessor processSignal initialSignal initialMetadata
    
    putStrLn "Original Signal:"
    print initialSignal
    putStrLn "Processed Signal:"
    print processedSignal
    putStrLn "Final Metadata:"
    print finalMetadata