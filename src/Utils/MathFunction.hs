{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Utils.MathFunction
    ( meanVector
    , stdVector
    , median
    , medianAbsoluteDeviation
    ) where

import Data.List (sort)
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra (Matrix, Vector)
import qualified Numeric.LinearAlgebra as LA

-- | Compute the mean of each column in the matrix.
meanVector :: Matrix Double -> Vector Double
meanVector mat = LA.scale (1 / fromIntegral (LA.rows mat)) (sumColumns mat)
  where
    -- | Compute the sum of each column in the matrix.
    sumColumns :: Matrix Double -> Vector Double
    sumColumns m = LA.fromList $ map LA.sumElements $ LA.toColumns m

-- | Compute the standard deviation of each column in the matrix.
stdVector :: Matrix Double -> Vector Double
stdVector mat = LA.cmap sqrt variances
  where
    -- Calculate the mean vector for the matrix
    means = meanVector mat
    -- Center the matrix by subtracting the mean from each column
    centeredMat = mat - LA.asRow means
    -- Calculate the variances for each column
    variances = LA.scale (1 / fromIntegral (LA.rows mat - 1)) (sumColumns (LA.cmap (** 2) centeredMat))

    -- | Compute the sum of each column in the matrix.
    sumColumns :: Matrix Double -> Vector Double
    sumColumns m = LA.fromList $ map LA.sumElements $ LA.toColumns m

median :: [Double] -> Maybe Double
median [] = Nothing
median xs = Just $ if odd n
                      then sorted !! mid
                      else (sorted !! (mid - 1) + sorted !! mid) / 2
  where
    sorted = sort xs
    n = length xs
    mid = n `div` 2

medianAbsoluteDeviation :: [Double] -> Maybe Double
medianAbsoluteDeviation [] = Nothing
medianAbsoluteDeviation xs = do
    med <- median xs
    let deviations = map abs (map (\x -> x - med) xs)
    median deviations
