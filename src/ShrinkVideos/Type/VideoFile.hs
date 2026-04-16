module ShrinkVideos.Type.VideoFile
    ( VideoFile (..)
    ) where

import System.OsPath (OsPath)

import ShrinkVideos.Type.FileSize (FileSize)
import ShrinkVideos.Type.VideoFormat (VideoFormat)

data VideoFile = VideoFile
    { videoFilePath :: !OsPath
    , videoFileFormat :: !VideoFormat
    , videoFileSize :: !FileSize
    }
    deriving (Eq, Show)
