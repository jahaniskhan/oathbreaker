{-# LANGUAGE OverloadedStrings #-}

module RFProcessor.SpectralAnalysis
    ( calculatePowerSpectralDensity
    , detectPeaks
    ) where

import Data.Complex (Complex(..), magnitude)
--import Numeric.LinearAlgebra (Vector, fromList, toList)
--import Data.List (foldl', sortOn)
--import Control.Monad (guard)

-- | Calculate the Power Spectral Density (PSD) of the signal
calculatePowerSpectralDensity :: [Complex Double] -> [Double]
calculatePowerSpectralDensity signal =
    let magnitudes = map magnitude signal
    in magnitudes  -- Placeholder implementation. Replace with actual PSD calculation.

-- | Detect peaks in the PSD
-- This function identifies local maxima in the PSD that exceed a certain threshold
detectPeaks :: [Double] -> Int -> [(Int, Double)]
detectPeaks psd windowSize =
    let indexedPsd = zip [0..] psd
        isPeak (i, v) = 
            let left = take windowSize $ reverse $ take i indexedPsd
                right = take windowSize $ drop (i + 1) indexedPsd
            in all (\(_, vl) -> v > vl) left && all (\(_, vr) -> v > vr) right
    in [ (i, v) | (i, v) <- indexedPsd, isPeak (i, v) ]
