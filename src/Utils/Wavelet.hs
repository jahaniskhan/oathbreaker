module Utils.Wavelet
    ( Wavelet(..)
    , WaveletError(..)
    , getWaveletCoefficients
    , getWaveletFilterPair
    ) where

-- | Enum to represent different wavelet types
data Wavelet = Haar
             | Daubechies Int  -- Int represents the order (e.g., 2 for db2)
             | Symlet Int      -- Symlet wavelets
             | Coiflet Int     -- Coiflet wavelets
             deriving (Show, Eq)

-- | Custom error type for wavelet-related errors
data WaveletError = UnsupportedWavelet Wavelet
                  | InvalidWaveletOrder Int
                  deriving (Show, Eq)

-- | Function to retrieve filter coefficients based on the wavelet type
getWaveletCoefficients :: Wavelet -> Either WaveletError [Double]
getWaveletCoefficients Haar = Right [1 / sqrt 2, 1 / sqrt 2]
getWaveletCoefficients (Daubechies 2) = Right
    [ (1 + sqrt 3) / (4 * sqrt 2)
    , (3 + sqrt 3) / (4 * sqrt 2)
    , (3 - sqrt 3) / (4 * sqrt 2)
    , (1 - sqrt 3) / (4 * sqrt 2)
    ]
-- Add other wavelets as needed
getWaveletCoefficients (Daubechies n)
    | n `elem` [4, 6, 8, 10] = Left $ InvalidWaveletOrder n  -- Placeholder
    | otherwise = Left $ UnsupportedWavelet (Daubechies n)
getWaveletCoefficients (Symlet n) = Left $ UnsupportedWavelet (Symlet n)
getWaveletCoefficients (Coiflet n) = Left $ UnsupportedWavelet (Coiflet n)

-- | Function to generate wavelet filter pairs (low-pass and high-pass)
getWaveletFilterPair :: Wavelet -> IO (FilterType, FilterType)
getWaveletFilterPair wavelet = do
    coeffs <- getWaveletCoefficients wavelet -- get wavelet coefficients
    let n = length coeffs -- number of coefficients
        lowPass = coeffs -- low pass filter is the same as the wavelet coefficients
        highPass = [(-1) ^ k * coeffs !! (n - k - 1) | k <- [0..n-1]]
    return (lowPass, highPass)
