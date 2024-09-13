module RFProcessor.SpectralAnalysis
    ( performFFT
    , calculatePowerSpectralDensity
    , detectPeaks
    , applyWindow
    , WindowType(..)
    ) where

import Data.Complex
import Data.List (sortOn)
import Data.Ord (Down(Down))

-- | Types of window functions
data WindowType
    = Rectangular
    | Hamming
    | Hanning
    deriving (Show, Eq)

-- | Apply a window function to a signal
applyWindow :: WindowType -> [Complex Double] -> [Complex Double]
applyWindow Rectangular signal = signal
applyWindow Hamming signal = zipWith (*) signal (map (:+ 0) hammingWindow)
    where
        n = length signal
        hammingWindow = [0.54 - 0.46 * cos (2 * pi * fromIntegral i / fromIntegral (n - 1)) | i <- [0..n-1]]
applyWindow Hanning signal = zipWith (*) signal (map (:+ 0) hanningWindow)
    where
        n = length signal
        hanningWindow = [0.5 * (1 - cos (2 * pi * fromIntegral i / fromIntegral (n - 1))) | i <- [0..n-1]]

-- | Perform Fast Fourier Transform (simplified implementation)
performFFT :: [Complex Double] -> [Complex Double]
performFFT [] = []
performFFT [x] = [x]
performFFT xs
  | n > 1 = combine (performFFT evens) (performFFT odds)
  | otherwise = xs
  where
    n = length xs
    (evens, odds) = splitAt (n `div` 2) xs
    combine es os = zipWith3 (\k e o -> e + exp (0 :+ (-2 * pi * k / fromIntegral n)) * o)
                             [0..] es (os ++ os)

-- | Calculate Power Spectral Density
calculatePowerSpectralDensity :: [Complex Double] -> [Double]
calculatePowerSpectralDensity signal =
    let fftResult = performFFT signal
        n = fromIntegral $ length signal
    in map (\x -> (magnitude x)^2 / n) fftResult

-- | Detect peaks in the spectrum
detectPeaks :: [Double] -> Int -> [(Int, Double)]
detectPeaks spectrum numPeaks =
    take numPeaks $ sortOn (Down . snd) $ filter isPeak $ zip [0..] spectrum
  where
    isPeak (i, v) = i > 0 && i < length spectrum - 1 &&
                    v > spectrum !! (i-1) && v > spectrum !! (i+1)
