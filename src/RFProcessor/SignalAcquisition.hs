module RFProcessor.SignalAcquisition
    ( performSignalPCA
    , sortIndices
    ) where

import Numeric.LinearAlgebra (Matrix, Vector, fromColumns, toColumns, toList)
import qualified Numeric.LinearAlgebra as LA
import Data.List (sortBy)
import Data.Ord (comparing)
import Utils.PCA (performPCA, PrincipalComponents(..))

-- | Performs Principal Component Analysis on the given data matrix.
--
-- Each row of the input matrix represents an observation,
-- and each column represents a feature.
--
-- Returns a tuple containing:
--   1. Transformed data matrix obtained by projecting the original data onto the principal components.
--   2. A vector of explained variance (eigenvalues) corresponding to each principal component.
performSignalPCA :: Matrix Double    -- ^ Data matrix (observations x features).
                -> Int               -- ^ Number of principal components to retain.
                -> Maybe (Matrix Double, Vector Double)
performSignalPCA dataMatrix numComponents = do
    PrincipalComponents { eigenvalues, eigenvectors } <- performPCA dataMatrix numComponents
    let principalComponents = LA.fromColumns eigenvectors
        transformedData    = dataMatrix LA.<> principalComponents
        explainedVariance  = LA.fromList eigenvalues
    return (transformedData, explainedVariance)

-- | Sorts the indices of the vector in descending order based on the vector's values.
--
-- Useful for identifying the top-k features or components.
--
-- Example:
--
-- >>> sortIndices (fromList [0.3, 0.1, 0.4, 0.2])
-- [2,0,3,1]
sortIndices :: Vector Double -> [Int]
sortIndices v = map snd $ sortBy (flip (comparing fst)) $ zip (toList v) [0..]
