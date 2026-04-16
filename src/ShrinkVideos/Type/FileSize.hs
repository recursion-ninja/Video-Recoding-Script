{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ShrinkVideos.Type.FileSize
    ( FileSize (..)
    , fileSizeToNatural
    , fileSizeFromIntegerMaybe
    , renderFileSizeSI
    ) where

import Numeric (showFFloat)
import Numeric.Natural (Natural)

newtype FileSize = FileSize Natural
    deriving (Eq, Ord, Num, Real, Enum, Integral)

instance Show FileSize where
    show (FileSize bytes) = show bytes

fileSizeToNatural :: FileSize -> Natural
fileSizeToNatural (FileSize bytes) = bytes

fileSizeFromIntegerMaybe :: Integer -> Maybe FileSize
fileSizeFromIntegerMaybe value
    | value < 0 = Nothing
    | otherwise = Just (FileSize (fromInteger value))

renderFileSizeSI :: FileSize -> String
renderFileSizeSI (FileSize bytes) = formatTo3 scaledValue ++ " " ++ unitLabel
  where
    units :: [String]
    units = ["B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"]

    (scaledValue, unitLabel) = scale (fromIntegral bytes :: Double) units

    scale :: Double -> [String] -> (Double, String)
    scale value [] = (value, "B")
    scale value [u] = (value, u)
    scale value (u:us)
        | value < 1024 = (value, u)
        | otherwise = scale (value / 1024) us

    formatTo3 :: Double -> String
    formatTo3 value = showFFloat (Just 3) value ""
