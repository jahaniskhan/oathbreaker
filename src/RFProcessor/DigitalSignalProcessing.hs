{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- This line is a special instruction to the Haskell compiler. It allows us to automatically
-- derive certain properties for our custom data types, making it easier to work with them.
-- Author: Jahan Khan
module SignalProcessing where

import Control.Monad.Reader
import Control.Monad.State
import Data.Complex

-- | Represents a signal as a list of complex numbers (I/Q data)
type Signal = [Complex Double]
-- A signal is a list of complex numbers, which are used to represent both the real and imaginary parts of a signal.

-- | Metadata for our signal
data SignalMetadata = SignalMetadata
    { sampleRate :: Double       -- ^ Sampling rate in Hz
    , centerFrequency :: Double  -- ^ Center frequency in Hz
    , gain :: Double             -- ^ Signal gain
    } deriving (Show)
-- This data structure holds information about the signal, such as how often it was sampled, its center frequency, and its gain.

-- | SignalT monad for managing signal metadata
newtype SignalT m a = SignalT { runSignalT :: StateT SignalMetadata m a }
    deriving (Functor, Applicative, Monad, MonadState SignalMetadata)
-- This is a special type that helps us manage the state of our signal processing operations. It's like a container that holds the state and the operation.

-- | RFProcessorT monad transformer for composable signal processing
newtype RFProcessorT m a = RFProcessorT { runRFProcessorT :: ReaderT Signal (SignalT m) a }
    deriving (Functor, Applicative, Monad, MonadReader Signal, MonadState SignalMetadata)
-- This is another special type that helps us process signals in a composable way. It's like a container that holds the signal and the operation that processes the signal.

-- | Helper function to run RFProcessorT
-- 
-- @param processor The RFProcessorT to run
-- @param signal The input signal
-- @param metadata The initial metadata
-- @return A tuple of the result and final metadata
runRFProcessor :: Monad m => RFProcessorT m a -> Signal -> SignalMetadata -> m (a, SignalMetadata)
runRFProcessor processor signal metadata = 
    runStateT (runReaderT (runRFProcessorT processor) signal) metadata
-- This function is a helper to run our signal processing operations. It takes the operation, the signal, and the initial metadata, and returns the result and the final metadata.

-- | Amplify the signal by a given factor
-- 
-- @param factor The amplification factor
-- @return The amplified signal
amplify :: Monad m => Double -> RFProcessorT m Signal
amplify factor = do
    signal <- ask
    return $ map (* (factor :+ 0)) signal
-- This function amplifies the signal by a given factor. It asks for the current signal, multiplies each element by the factor, and returns the amplified signal.

-- | Perform a Discrete Fourier Transform on the signal
-- 
-- @return The transformed signal
dft :: Monad m => RFProcessorT m Signal
dft = do
    signal <- ask
    let n = length signal
        k = [0..n-1]
        omega = (-2 * pi) :+ 0
    return [sum [x * exp((omega * fromIntegral (j*k)) / fromIntegral n) | (x, j) <- zip signal [0..]] | k <- k]
-- This function performs a Discrete Fourier Transform on the signal. It asks for the current signal, calculates the DFT, and returns the transformed signal.

-- | Update the signal metadata
-- 
-- @param f The function to update the metadata
updateMetadata :: Monad m => (SignalMetadata -> SignalMetadata) -> RFProcessorT m ()
updateMetadata f = RFProcessorT $ lift $ modify f
-- This function updates the metadata of the signal. It takes a function that modifies the metadata, and applies it to the current metadata.

-- | Example of a more complex processing pipeline
processSignal :: Monad m => RFProcessorT m Signal
processSignal = do
    amplify 2.0
    updateMetadata (\md -> md { gain = gain md * 2.0 })
    dft
