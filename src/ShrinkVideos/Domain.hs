{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ShrinkVideos.Domain
    ( Verbosity (..)
    , defaultVerbosity
    , LogLevel (..)
    , shouldLog
    , isX265Codec
    , RecodeDecision (..)
    , decideRecodeDecision
    , RecodeResult (..)
    , Summary (..)
    , emptySummary
    , setSummaryFilesFound
    , addRecodeResult
    ) where

import Data.Text (Text)

import Data.Text qualified as T

import ShrinkVideos.Type.FileSize (FileSize)

data Verbosity
    = Quiet
    | ErrorsOnly
    | WarningsAndErrors
    | Informational
    | ExtraDetail
    deriving (Eq, Ord, Show, Enum, Bounded)

defaultVerbosity :: Verbosity
defaultVerbosity = Informational

data LogLevel
    = LevelError
    | LevelWarning
    | LevelInfo
    | LevelExtra
    deriving (Eq, Ord, Show, Enum, Bounded)

shouldLog :: Verbosity -> LogLevel -> Bool
shouldLog Quiet _ = False
shouldLog ErrorsOnly level = level <= LevelError
shouldLog WarningsAndErrors level = level <= LevelWarning
shouldLog Informational level = level <= LevelInfo
shouldLog ExtraDetail _ = True

isX265Codec :: Text -> Bool
isX265Codec codecName =
    normalized == "hevc"
        || normalized == "x265"
        || normalized == "libx265"
  where
    normalized = T.toLower (T.strip codecName)

data RecodeDecision
    = KeepEncodedOutput
    | ReencodeCopyForExtension
    deriving (Eq, Ord, Show)

decideRecodeDecision :: FileSize -> FileSize -> RecodeDecision
decideRecodeDecision originalBytes encodedBytes
    | encodedBytes <= originalBytes = KeepEncodedOutput
    | otherwise = ReencodeCopyForExtension

data RecodeResult = RecodeResult
    { recodeResultOriginalBytes :: !FileSize
    , recodeResultFinalBytes :: !FileSize
    }
    deriving (Eq, Show)

data Summary = Summary
    { summaryFilesFound :: !Word
    , summaryFilesReencoded :: !Word
    , summaryOriginalBytes :: !FileSize
    , summaryFinalBytes :: !FileSize
    }
    deriving (Eq, Show)

emptySummary :: Summary
emptySummary =
    Summary
        { summaryFilesFound = 0
        , summaryFilesReencoded = 0
        , summaryOriginalBytes = 0
        , summaryFinalBytes = 0
        }

setSummaryFilesFound :: Word -> Summary -> Summary
setSummaryFilesFound filesFound summary =
    summary { summaryFilesFound = filesFound }

addRecodeResult :: RecodeResult -> Summary -> Summary
addRecodeResult RecodeResult{..} summary =
    summary
        { summaryFilesReencoded = summaryFilesReencoded summary + 1
        , summaryOriginalBytes = summaryOriginalBytes summary + recodeResultOriginalBytes
        , summaryFinalBytes = summaryFinalBytes summary + recodeResultFinalBytes
        }
