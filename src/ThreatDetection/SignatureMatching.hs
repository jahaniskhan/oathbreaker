module ThreatDetection.SignatureMatching
    ( matchSignatures
    , matchSignatureDTW
    , Signature(..)
    , MatchResult(..)
    ) where

import Data.Complex
import Data.List (isInfixOf, minimumBy)
import Data.Function (on)

-- | Represents a known signal signature
data Signature = Signature
    { signatureName :: String
    , signaturePattern :: [Complex Double]
    } deriving (Show)

-- | Represents the result of a signature match
data MatchResult = MatchResult
    { matchedSignature :: Signature
    , matchPosition :: Int
    , matchDistance :: Double
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
dtw :: [Complex Double] -> [Complex Double] -> (Double, Int)
dtw signal pattern =
    let n = length signal
        m = length pattern
        cost i j = magnitude (signal !! i - pattern !! j)
        dp = [[if i == 0 && j == 0 then 0
               else if i == 0 then dp !! 0 !! (j-1) + cost 0 j
               else if j == 0 then dp !! (i-1) !! 0 + cost i 0
               else minimum [ dp !! (i-1) !! j      + cost i j
                            , dp !! i     !! (j-1)  + cost i j
                            , dp !! (i-1) !! (j-1)  + cost i j
                            ]
              | j <- [0..m-1]] | i <- [0..n-1]]
        distances = [((dp !! i !! (m-1)), i) | i <- [0..n-1]]
    in minimumBy (compare `on` fst) distances

-- | Find local minima in a list
findMinima :: [Double] -> [Int]
findMinima xs = [i | (i, x) <- zip [0..] xs, isLocalMin i xs]
  where
    isLocalMin i xs = (i == 0 || xs !! (i-1) > xs !! i) && (i == length xs - 1 || xs !! (i+1) > xs !! i)
