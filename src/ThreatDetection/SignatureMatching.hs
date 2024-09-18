module ThreatDetection.SignatureMatching
    ( matchSignatures
    , matchSignatureDTW
    , Signature(..)
    , MatchResult(..)
    ) where

import Data.Complex
import Data.Function (on)
import Data.List (minimumBy)
import qualified Data.Vector as V

-- | Represents a known signal signature
data Signature = Signature
    { signatureName    :: String
    , signaturePattern :: [Complex Double]
    } deriving (Show)

-- | Represents the result of a signature match
data MatchResult = MatchResult
    { matchedSignature :: Signature
    , matchPosition    :: Int
    , matchDistance    :: Double
    } deriving (Show)

-- | Matches known signatures against the signal
matchSignatures :: [Signature] -> [Complex Double] -> [MatchResult]
matchSignatures signatures signal = concatMap (matchSignature signal) signatures

-- | Matches a single signature against the signal
matchSignature :: [Complex Double] -> Signature -> [MatchResult]
matchSignature signal signature =
    let pattern = signaturePattern signature
        positions = findSublistIndices pattern signal
    in map (\pos -> MatchResult signature pos 0) positions

-- | Finds all starting indices of a sublist in a list
findSublistIndices :: Eq a => [a] -> [a] -> [Int]
findSublistIndices sublist list = [i | i <- [0..length list - length sublist], take (length sublist) (drop i list) == sublist]

-- | Matches a single signature against the signal using DTW
matchSignatureDTW :: [Complex Double] -> Signature -> MatchResult
matchSignatureDTW signal signature =
    let pattern = signaturePattern signature
        (distance, position) = dtw signal pattern
    in MatchResult signature position distance

-- | Dynamic Time Warping algorithm
dtw :: V.Vector (Complex Double) -> V.Vector (Complex Double) -> (Double, Int)
dtw signal pattern =
    let n = V.length signal
        m = V.length pattern
        cost i j = magnitude (signal V.! i - pattern V.! j)
        dp = V.generate n $ \i ->
            V.generate m $ \j ->
                if i == 0 && j == 0 then cost 0 0
                else if i == 0 then (dp V.! i V.! (j - 1)) + cost i j
                else if j == 0 then (dp V.! (i - 1) V.! j) + cost i j
                else minimum [ (dp V.! (i - 1) V.! j) + cost i j
                             , (dp V.! i V.! (j - 1)) + cost i j
                             , (dp V.! (i - 1) V.! (j - 1)) + cost i j
                             ]
        distances = [ (dp V.! i V.! (m - 1), i) | i <- [0..n-1] ]
    in minimumBy (compare `on` fst) distances
