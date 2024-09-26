{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Utils.MathFunction
    ( median
    , medianAbsoluteDeviation
    , stddev
    , meanVector
    , stdVector
    ) where

import Data.List (sort)
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra (Matrix, Vector)

-- | Calculate the median of a list.
-- Returns Nothing for an empty list.
median :: (Ord a, Fractional a) => [a] -> Maybe a
median xs
  | null sorted = Nothing
  | odd len     = Just $ sorted !! mid
  | otherwise   = Just $ (sorted !! (mid - 1) + sorted !! mid) / 2
  where
    sorted = sort xs
    len    = length sorted
    mid    = len `div` 2

-- | Calculate the Median Absolute Deviation (MAD) of a list.
-- MAD is a robust measure of statistical dispersion.
medianAbsoluteDeviation :: [Double] -> Maybe Double
medianAbsoluteDeviation xs = do
    med <- median xs
    let deviations = map (abs . subtract med) xs
    median deviations

-- | Calculate the sample standard deviation of a list.
-- Returns a small epsilon value for lists with fewer than two elements or zero variance to prevent division by zero.
stddev :: [Double] -> Double
stddev xs
  | length xs < 2 = epsilon        -- Use a small epsilon to avoid division by zero
  | variance == 0 = epsilon        -- Handle zero variance
  | otherwise     = sqrt variance
  where
    n = length xs
    meanVal = sum xs / fromIntegral n
    variance = sum (map (\x -> (x - meanVal) ** 2) xs) / fromIntegral (n - 1)
    epsilon = 1e-8                 -- Small epsilon value to prevent division by zero

-- | Compute the mean of each column in the matrix.
meanVector :: Matrix Double -> Vector Double
meanVector mat = LA.scale (1 / fromIntegral (LA.rows mat)) (sumColumns mat)
  where
    -- | Compute the sum of each column in the matrix.
    sumColumns :: Matrix Double -> Vector Double
    sumColumns m = LA.fromList $ map LA.sumElements $ LA.toColumns m

-- | Compute the standard deviation of each column in the matrix.
stdVector :: Matrix Double -> Vector Double
stdVector mat = LA.cmap sqrt variances  -- Replaced LA.^ with LA.cmap sqrt
  where
    means = meanVector mat
    centeredMat = mat - LA.asRow means
    variances = LA.scale (1 / fromIntegral (LA.rows mat - 1)) (sumColumns (LA.cmap (^2) centeredMat))  -- Replaced LA.sumRows and LA.^2 with sumColumns and LA.cmap (^2)

    -- | Compute the sum of each column in the matrix.
    sumColumns :: Matrix Double -> Vector Double
    sumColumns m = LA.fromList $ map LA.sumElements $ LA.toColumns m
