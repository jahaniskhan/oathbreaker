{-# LANGUAGE FlexibleContexts #-}

module ThreatDetection.SignatureMatching
    ( matchSignatures
    , matchSignatureDTW
    , Signature(..)
    , MatchResult(..)
    ) where

import Data.Complex (Complex, magnitude)
import qualified Data.Vector as V
import Control.Monad (forM_)
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad.ST
import qualified Data.Array.Unboxed as U
import Data.List (minimumBy)
import Numeric.LinearAlgebra (Vector, fromList, toList)
import qualified Numeric.LinearAlgebra as LA

-- | Represents a known signal signature representing a known pattern and a list of complex numbers
data Signature = Signature
    { signatureName    :: String -- ^ Name of the signature
    , signaturePattern :: [Complex Double] -- ^ The pattern of the signature/list of complex numbers
    } deriving (Show, Eq)

-- | Represents the result of a signature match
data MatchResult = MatchResult -- Data type to hold the result of a signature match
    { matchedSignature :: Signature -- ^ The matched signature
    , matchPosition    :: Int -- ^ The position of the match
    , matchDistance    :: Double -- ^ The distance of the match
    } deriving (Show, Eq)

-- | Dynamic Time Warping (DTW) implementation
dtw :: V.Vector (Complex Double) -> V.Vector (Complex Double) -> Double
dtw s t = dtwMatrix U.! (n - 1, m - 1)
  where
    n = V.length s
    m = V.length t
    dtwMatrix :: UArray (Int, Int) Double
    dtwMatrix = runSTUArray $ do
        mat <- newArray ((0, 0), (n - 1, m - 1)) (1/0)
        writeArray mat (0, 0) (distance (s V.! 0) (t V.! 0))
        forM_ [1..n - 1] $ \i -> do
            cost <- distanceM mat (i - 1, 0) (s V.! i) (t V.! 0)
            writeArray mat (i, 0) cost
        forM_ [1..m - 1] $ \j -> do
            cost <- distanceM mat (0, j - 1) (s V.! 0) (t V.! j)
            writeArray mat (0, j) cost
        forM_ [1..n - 1] $ \i ->
            forM_ [1..m - 1] $ \j -> do
                costCurrent <- return $ distance (s V.! i) (t V.! j)
                up <- readArray mat (i - 1, j)
                left <- readArray mat (i, j - 1)
                diag <- readArray mat (i - 1, j - 1)
                let minPrev = minimum [up, left, diag]
                    cost = costCurrent + minPrev
                writeArray mat (i, j) cost
        return mat

    distance :: Complex Double -> Complex Double -> Double
    distance a b = magnitude (a - b)

    distanceM :: STUArray s (Int, Int) Double -> (Int, Int) -> Complex Double -> Complex Double -> ST s Double
    distanceM mat (i, j) a b = do
        let dist = distance a b
        return $ dist
        -- Alternatively, you can include weights or other distance metrics

-- | Find the best match position in the signal for the given pattern using DTW
findBestMatchPosition :: [Complex Double] -> [Complex Double] -> Int
findBestMatchPosition signal pattern
    | length signal < length pattern = error "Signal is shorter than the pattern."
    | otherwise = 
        let n = length signal
            m = length pattern
            positions = [0 .. n - m]
            computeDistance i =
                let window = take m $ drop i signal
                    windowVec = V.fromList window
                    patternVec = V.fromList pattern
                    distanceValue = dtw windowVec patternVec
                in (i, distanceValue)
            distances = map computeDistance positions
            (bestPosition, _) = minimumBy (\a b -> compare (snd a) (snd b)) distances
        in bestPosition

-- | Matches a known signature against the signal using DTW
matchSignatureDTW :: [Complex Double] -> Signature -> MatchResult
matchSignatureDTW signal signature =
    let position = findBestMatchPosition signal (signaturePattern signature)
        window = take (length $ signaturePattern signature) $ drop position signal
        windowVec = V.fromList window
        patternVec = V.fromList (signaturePattern signature)
        distanceValue = dtw windowVec patternVec
    in MatchResult signature position distanceValue

-- | Matches known signatures against the signal
matchSignatures :: [Signature] -> [Complex Double] -> [MatchResult]
matchSignatures signatures signal = 
    map (matchSignatureDTW signal) signatures
