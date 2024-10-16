{-# LANGUAGE OverloadedStrings #-}

module RFProcessor.SpectralAnalysis
    ( calculatePowerSpectralDensity
    , detectPeaks
    ) where

import Data.Complex (Complex(..), magnitude)
import Numeric.GSL.Fourier (fft)
import Numeric.LinearAlgebra (Vector, fromList, toList, cmap, size)
import qualified Numeric.LinearAlgebra as LA

-- | Apply a Hann window to the signal
hannWindow :: Vector (Complex Double) -> Vector (Complex Double)
hannWindow signal =
    let n = size signal
        indices = [0 .. n - 1]
        windowFunc i = 0.5 * (1 - cos (2 * pi * fromIntegral i / fromIntegral (n - 1)))
        window = fromList $ map windowFunc indices       -- Vector Double
        windowC = LA.cmap (\x -> x :+ 0) window        -- Convert to Vector (Complex Double)
    in LA.zipWith (*) signal windowC  -- Element-wise multiplication

-- | Calculate the Power Spectral Density (PSD) of the signal using FFT.
calculatePowerSpectralDensity :: [Complex Double] -> [Double]
calculatePowerSpectralDensity signal =
    let vectorSignal = LA.fromList signal                                -- Convert list to Vector
        windowedSignal = hannWindow vectorSignal                      -- Apply Hann window
        fftResult = fft windowedSignal                                -- Perform FFT
        magnitudeSquared = LA.cmap (\x -> magnitude x ** 2) fftResult    -- Calculate |X(f)|^2
        n = fromIntegral $ LA.size vectorSignal                          -- Number of samples
        psd = toList $ LA.cmap (/ n) magnitudeSquared                    -- Divide by N for proper scaling
    in psd

-- | Detect peaks in the PSD
-- This function identifies local maxima in the PSD that exceed a certain threshold
detectPeaks :: [Double]          -- ^ PSD values
           -> Int               -- ^ Window size for peak detection
           -> [(Int, Double)]   -- ^ List of peaks with their indices and magnitudes
detectPeaks psd windowSize =
    let indexedPsd = zip [0..] psd
        isPeak (i, v) =
            let left = take windowSize $ reverse $ take i indexedPsd
                right = take windowSize $ drop (i + 1) indexedPsd
            in all (\(_, val) -> val < v) left && all (\(_, val) -> val < v) right
    in filter isPeak indexedPsd
