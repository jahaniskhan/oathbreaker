{-# LANGUAGE FlexibleContexts #-}

module Utils.PCA
    ( performPCA
    , PrincipalComponents(..)
    ) where

import Numeric.LinearAlgebra
    ( Matrix, Vector, eigSH, (<>), tr, fromColumns, toColumns, toList )
import qualified Numeric.LinearAlgebra as LA
import Data.List (sortBy)
import Data.Ord (comparing)

-- | Represents the principal components resulting from PCA.
data PrincipalComponents = PrincipalComponents
    { eigenvalues  :: Vector Double
    , eigenvectors :: Matrix Double
    } deriving (Show, Eq)

-- | Performs Principal Component Analysis on the given data matrix.
-- Returns the specified number of principal components.
performPCA :: Matrix Double -> Int -> Maybe PrincipalComponents
performPCA mat numComponents
    | numComponents <= 0 || numComponents > min (rows mat) (cols mat) = Nothing
    | otherwise = Just $ PrincipalComponents
        { eigenvalues  = sortedEigenvalues
        , eigenvectors = sortedEigenvectors
        }
  where
    -- Step 1: Mean-center the data
    matCentered = mat - repmat (meanColumns mat) (rows mat) 1

    -- Step 2: Compute covariance matrix
    covarianceMatrix = (tr matCentered <> matCentered) / fromIntegral (rows mat - 1)

    -- Step 3: Eigen decomposition
    (eigenvalues', eigenvectors') = eigSH covarianceMatrix

    -- Step 4: Sort eigenvalues and eigenvectors in descending order
    eigenPairs = zip (toList eigenvalues') (toColumns eigenvectors')
    sortedEigenPairs = reverse $ sortBy (comparing fst) eigenPairs
    (sortedEigenvaluesList, sortedEigenvectorsList) = unzip sortedEigenPairs
    sortedEigenvalues = LA.fromList $ take numComponents sortedEigenvaluesList
    sortedEigenvectors = fromColumns $ take numComponents sortedEigenvectorsList

-- Helper functions
rows :: Matrix Double -> Int
rows = LA.rows

cols :: Matrix Double -> Int
cols = LA.cols

meanColumns :: Matrix Double -> Vector Double
meanColumns m = LA.mean m LA.<> LA.ones (LA.cols m)

repmat :: Vector Double -> Int -> Int -> Matrix Double
repmat v r c = LA.fromColumns $ replicate c v
