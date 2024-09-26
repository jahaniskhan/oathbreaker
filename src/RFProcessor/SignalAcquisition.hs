{-# LANGUAGE NamedFieldPuns #-}

module RFProcessor.SignalAcquisition
    ( performSignalPCA
    , sortIndices
    ) where

import Numeric.LinearAlgebra (Matrix, Vector)
import qualified Numeric.LinearAlgebra as LA
import Utils.PCA (performPCA, PrincipalComponents(..))
import Data.List (sortBy)
import Data.Ord (comparing)

-- | Performs PCA on the signal data matrix and returns transformed data and explained variance.
performSignalPCA :: LA.Matrix Double  -- ^ Data matrix (observations x features).
                 -> Int               -- ^ Number of principal components to retain.
                 -> Either String (LA.Matrix Double, LA.Vector Double)
performSignalPCA dataMatrix numComponents = do
    PrincipalComponents { eigenvalues, eigenvectors } <- performPCA dataMatrix numComponents
    let principalComponents = LA.fromColumns (LA.toColumns eigenvectors)
        transformedData     = dataMatrix LA.<> principalComponents
    return (transformedData, eigenvalues)

-- | Sorts the indices of the vector in descending order based on the vector's values.
--
-- Useful for identifying the top-k features or components.
--
-- Example:
--
-- >>> sortIndices (fromList [0.3, 0.1, 0.4, 0.2])
-- [2,0,3,1]
sortIndices :: Vector Double -> [Int]
sortIndices v = map snd $ sortBy (flip (comparing fst)) $ zip (LA.toList v) [0..]
