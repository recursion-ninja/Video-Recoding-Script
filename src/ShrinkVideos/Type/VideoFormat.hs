module ShrinkVideos.Type.VideoFormat
    ( VideoFormat (..)
    , defaultVideoFormat
    , renderVideoFormat
    , parseVideoFormat
    , toOsPath
    , isVideoFile
    , ffmpegFormat
    ) where

import Data.Char (isSpace, toLower)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))

import Data.List.NonEmpty qualified as NE
import System.OsPath (OsPath)
import System.OsPath qualified as Path

data VideoFormat
    = Format3g2
    | Format3gp
    | Amv
    | Asf
    | Avi
    | F4a
    | F4b
    | F4p
    | F4v
    | Flv
    | Gifv
    | M4p
    | M4v
    | Mkv
    | Mng
    | Mov
    | Mp2
    | Mp4
    | Mpe
    | Mpeg
    | Mpg
    | Mpv
    | Mxf
    | Nsv
    | Ogg
    | Ogv
    | Qt
    | Rm
    | Roq
    | Rrc
    | Svi
    | Vob
    | Webm
    | Wmv
    | Yuv
    deriving (Bounded, Enum, Eq, Ord, Show)

defaultVideoFormat :: VideoFormat
defaultVideoFormat = Mp4

renderVideoFormat :: VideoFormat -> String
renderVideoFormat = renderFileExtension

parseVideoFormat :: String -> Either String VideoFormat
parseVideoFormat rawInput =
    let strip = let f = dropWhile isSpace in reverse . f . reverse . f
        normalized = fmap toLower . dropWhile (== '.') . strip $ rawInput
    in  case normalized of
            [] -> Left "Output format cannot be empty."
            formatText ->
                case find ((== formatText) . renderVideoFormat) (NE.toList allVideoFormats) of
                    Nothing ->
                        Left $
                            "Output format '"
                                <> formatText
                                <> "' is not a recognized video format."
                    Just videoFormat -> Right videoFormat

toOsPath :: VideoFormat -> OsPath
toOsPath = Path.unsafeEncodeUtf . renderFileExtension

isVideoFile :: OsPath -> Maybe VideoFormat
isVideoFile path =
    case Path.unpack (Path.takeExtension path) of
        [] -> Nothing
        x:xs ->
            let extensionPath
                    | x == Path.extSeparator = Path.pack xs
                    | otherwise = Path.takeExtension path
            in  find ((== extensionPath) . toOsPath) (NE.toList allVideoFormats)

ffmpegFormat :: VideoFormat -> String
ffmpegFormat videoFormat =
    case videoFormat of
        Format3g2 -> "3g2"
        Format3gp -> "3gp"
        Amv -> "amv"
        Asf -> "asf"
        Avi -> "avi"
        F4a -> "f4v"
        F4b -> "f4v"
        F4p -> "f4v"
        F4v -> "f4v"
        Flv -> "flv"
        Gifv -> "mp4"
        M4p -> "mp4"
        M4v -> "mp4"
        Mkv -> "matroska"
        Mng -> "matroska"
        Mov -> "mov"
        Mp2 -> "mpeg"
        Mp4 -> "mp4"
        Mpe -> "mpeg"
        Mpeg -> "mpeg"
        Mpg -> "mpeg"
        Mpv -> "mpeg"
        Mxf -> "mxf"
        Nsv -> "matroska"
        Ogg -> "ogg"
        Ogv -> "ogv"
        Qt -> "mov"
        Rm -> "rm"
        Roq -> "roq"
        Rrc -> "matroska"
        Svi -> "3gp"
        Vob -> "vob"
        Webm -> "webm"
        Wmv -> "asf"
        Yuv -> "rawvideo"

allVideoFormats :: NonEmpty VideoFormat
allVideoFormats = minBound :| [succ minBound .. maxBound]

renderFileExtension :: VideoFormat -> String
renderFileExtension videoFormat =
    case videoFormat of
        Format3g2 -> "3g2"
        Format3gp -> "3gp"
        Amv -> "amv"
        Asf -> "asf"
        Avi -> "avi"
        F4a -> "f4a"
        F4b -> "f4b"
        F4p -> "f4p"
        F4v -> "f4v"
        Flv -> "flv"
        Gifv -> "gifv"
        M4p -> "m4p"
        M4v -> "m4v"
        Mkv -> "mkv"
        Mng -> "mng"
        Mov -> "mov"
        Mp2 -> "mp2"
        Mp4 -> "mp4"
        Mpe -> "mpe"
        Mpeg -> "mpeg"
        Mpg -> "mpg"
        Mpv -> "mpv"
        Mxf -> "mxf"
        Nsv -> "nsv"
        Ogg -> "ogg"
        Ogv -> "ogv"
        Qt -> "qt"
        Rm -> "rm"
        Roq -> "roq"
        Rrc -> "rrc"
        Svi -> "svi"
        Vob -> "vob"
        Webm -> "webm"
        Wmv -> "wmv"
        Yuv -> "yuv"
