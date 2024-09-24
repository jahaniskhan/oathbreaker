module Utils.MathFunction
    ( median
    , medianAbsoluteDeviation
    , stddev
    ) where

import Data.List (sort)

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
    n = fromIntegral (length xs)
    meanVal = sum xs / n
    variance = (sum (map (\x -> (x - meanVal) ^ 2) xs)) / (fromIntegral n - 1)
    epsilon = 1e-8                 -- Small epsilon value to prevent division by zero
