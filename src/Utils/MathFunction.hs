module Utils.MathFunctions
    ( median
    , medianAbsoluteDeviation
    ) where

import Data.List (sort)

-- | Calculate the median of a list.
-- Returns Nothing for an empty list.
median :: (Ord a, Fractional a) => [a] -> Maybe a
median xs
  | null sorted = Nothing
  | odd len    = Just $ sorted !! mid
  | otherwise  = Just $ (sorted !! (mid - 1) + sorted !! mid) / 2
  where
    sorted = sort xs
    len    = length sorted
    mid    = len `div` 2

-- | Calculate the Median Absolute Deviation (MAD) of a list.
-- MAD is a robust measure of statistical dispersion.
medianAbsoluteDeviation :: [Double] -> Maybe Double
medianAbsoluteDeviation xs = do
    med <- median xs
    let deviations = map (abs . ( - med)) xs
    median deviations