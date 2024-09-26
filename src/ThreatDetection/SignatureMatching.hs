{-# LANGUAGE FlexibleContexts #-}

module ThreatDetection.SignatureMatching -- declaring the module
    ( matchSignatures -- exporting the function
    , matchSignatureDTW -- exporting the function
    , Signature(..) -- exporting the data type
    , MatchResult(..) -- exporting the data type
    ) where

import Data.Complex (Complex, magnitude) -- importing the necessary modules
import qualified Data.Vector as V -- importing the necessary modules
import Control.Monad (forM_) -- importing the necessary modules
import Data.Array.ST -- importing the necessary modules
import Data.Array.Unboxed
import Control.Monad.ST
import qualified Data.Array.Unboxed as U
import Data.List (minimumBy)

-- | Represents a known signal signature represeting a known pattern and a list of complex numbers
data Signature = Signature
    { signatureName    :: String -- name of the signature
    , signaturePattern :: [Complex Double] -- the pattern of the signature/list of complex numbers
    } deriving (Show, Eq) -- deriving the data type, type classes to show and compare for equality

-- | Represents the result of a signature match
data MatchResult = MatchResult -- data type to hold the result of a signature match
    { matchedSignature :: Signature -- the matched signature
    , matchPosition    :: Int -- the position of the match
    , matchDistance    :: Double -- the distance of the match
    } deriving (Show, Eq)

-- | Dynamic Time Warping (DTW) implementation
dtw :: V.Vector (Complex Double) -> V.Vector (Complex Double) -> Double --inputs: s -> vector of complex numbers of the signature pattern, t -> vector of complex numbers, segment of the signal
dtw s t = dtwMatrix U.! (n - 1, m - 1) -- retrieves the value at the bottom-right corner of the cost matrix
  where
    n = V.length s -- length of the signature pattern and signal vectors
    m = V.length t
    dtwMatrix :: UArray (Int, Int) Double -- cost matrix unboxed array with integer indices and storing double values
    dtwMatrix = runSTUArray $ do --computation in the state thread monad, mutable operations, immutable unboxed array
        mat <- newArray ((0, 0), (n - 1, m - 1)) (1/0)  -- Initialize with infinity
        writeArray mat (0, 0) (distance (s V.! 0) (t V.! 0)) -- sets the first cell (0,0) to the distance between the first elements of s and t
        forM_ [1..n - 1] $ \i -> do -- iterates over the rows of the matrix starting from (1,0)
            cost <- distanceM mat (i - 1, 0) (s V.! i) (t V.! 0) -- calculates the cost for the current cell and sets it in the matrix
            writeArray mat (i, 0) cost -- sets the cost for the current cell in the matrix  
        forM_ [1..m - 1] $ \j -> do -- iterates over the columns of the matrix starting from (0,1)
            cost <- distanceM mat (0, j - 1) (s V.! 0) (t V.! j) -- calculates the cost for the current cell and sets it in the matrix  
            writeArray mat (0, j) cost -- sets the cost for the current cell in the matrix  
        forM_ [1..n - 1] $ \i -> -- iterates over the rows of the matrix starting from (1,1)
            forM_ [1..m - 1] $ \j -> do -- iterates over the columns of the matrix starting from (1,1)
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
        if i == 0 || j == 0
            then return $ distance a b
            else do
                prevCost <- readArray mat (i - 1, j - 1) -- Adjusted to read a valid previous cell
                return $ prevCost + distance a b

-- | Find the best match position in the signal for the given pattern using DTW
findBestMatchPosition :: [Complex Double] -> [Complex Double] -> Int -- inputs: signal -> list of complex numbers, pattern -> list of complex numbers
findBestMatchPosition signal pattern
    | length signal < length pattern = error "Signal is shorter than the pattern."
    | otherwise = 
        let n = length signal -- length of the signal
            m = length pattern -- length of pattern 
            positions = [0 .. n - m]
            computeDistance i =
                let window = take m $ drop i signal  -- Extract subsequence of length m
                    windowVec = V.fromList window
                    patternVec = V.fromList pattern
                    distanceValue = dtw windowVec patternVec
                in (i, distanceValue)
            distances = map computeDistance positions
            (bestPosition, _) = minimumBy (\a b -> compare (snd a) (snd b)) distances
        in bestPosition


-- | Matches a known signature against the signal using DTW = represents the result of matching a signature against a signal
matchSignatureDTW :: [Complex Double] -> Signature -> MatchResult -- inputs: signal -> list of complex numbers, signature -> signature data type
matchSignatureDTW signal signature =
    let position = findBestMatchPosition signal (signaturePattern signature)
        window = take (length $ signaturePattern signature) $ drop position signal
        windowVec = V.fromList window
        patternVec = V.fromList (signaturePattern signature)
        distanceValue = dtw windowVec patternVec
    in MatchResult signature position distanceValue

-- | Matches known signatures against the signal
matchSignatures :: [Signature] -> [Complex Double] -> [MatchResult] -- inputs: signatures -> list of signatures, signal -> list of complex numbers
matchSignatures signatures signal = 
    map (matchSignatureDTW signal) signatures -- maps the matchSignatureDTW function over the list of signatures and returns a list of match results
