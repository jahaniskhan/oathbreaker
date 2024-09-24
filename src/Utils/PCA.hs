{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Utils.PCA
    ( performPCA
    , PrincipalComponents(..)
    ) where

import Prelude hiding ((<>))
import qualified Numeric.LinearAlgebra as LA
import Data.List (sortBy)
import Data.Ord (comparing)

-- | Represents the principal components resulting from PCA.
data PrincipalComponents = PrincipalComponents
    { eigenvalues  :: LA.Vector Double
    -- ^ A vector containing the eigenvalues, indicating the variance captured by each principal component.
    , eigenvectors :: LA.Matrix Double
    -- ^ A matrix where each column is an eigenvector corresponding to a principal component.
    } deriving (Show, Eq)

-- | Performs Principal Component Analysis on the given data matrix.
-- Returns the specified number of principal components.
performPCA :: LA.Matrix Double -> Int -> Either String PrincipalComponents
performPCA mat numComponents
    | numComponents <= 0 = Left "Number of components must be positive."
    | numComponents > LA.cols mat = Left "Number of components exceeds the number of features."
    | otherwise =
        let
            -- Step 1: Mean Centering
            meanVec = meanColumns mat
            matCentered = centerMatrix mat meanVec

            -- Step 2: Covariance Matrix
            covarianceMatrix = (LA.tr matCentered LA.<> matCentered) / fromIntegral (LA.rows mat - 1)

            -- Step 3: Eigen Decomposition
            (eigenvalues', eigenvectors') = LA.eigSH covarianceMatrix

            -- Step 4: Sorting Eigenvalues and Eigenvectors
            eigenPairs = zip (LA.toList eigenvalues') (LA.toColumns eigenvectors')
            sortedEigenPairs = reverse $ sortBy (comparing fst) eigenPairs
            (sortedEigenvaluesList, sortedEigenvectorsList) = unzip sortedEigenPairs

            -- Step 5: Selecting Top Principal Components
            sortedEigenvalues = LA.fromList $ take numComponents sortedEigenvaluesList
            sortedEigenvectors = LA.fromColumns $ take numComponents sortedEigenvectorsList
        in
            Right $ PrincipalComponents sortedEigenvalues sortedEigenvectors

-- | Centers the matrix by subtracting the mean vector from each row.
centerMatrix :: LA.Matrix Double -> LA.Vector Double -> LA.Matrix Double
centerMatrix mat meanVec = mat - LA.asRow meanVec

-- | Calculates the mean of each column in the matrix.
meanColumns :: LA.Matrix Double -> LA.Vector Double
meanColumns m = LA.scale (1 / fromIntegral (LA.rows m)) columnSums
  where
    transposed = LA.tr m
    columnSums = LA.foldRowWise (+) transposed
