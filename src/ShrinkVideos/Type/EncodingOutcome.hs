{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ShrinkVideos.Type.EncodingOutcome
    ( EncodingOutcome (..)
    ) where

import Data.Time.Clock (UTCTime)

import ShrinkVideos.Type.FileSize (FileSize)

data EncodingOutcome = EncodingOutcome
    { encodingOutcomeOriginalFileSize :: !FileSize
    , encodingOutcomeFinishedFileSize :: !FileSize
    , encodingOutcomeStartedAt :: !UTCTime
    , encodingOutcomeStoppedAt :: !UTCTime
    }
    deriving (Eq, Show)
