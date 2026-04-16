module ShrinkVideos.Cli
    ( Options (..)
    , parseOptions
    ) where

import Control.Applicative (asum, many)
import Options.Applicative
    ( Parser
    , ParserInfo
    , eitherReader
    , execParser
    , flag'
    , fullDesc
    , header
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , progDesc
    , short
    , showDefaultWith
    , str
    , switch
    , value
    , (<**>)
    )
import System.OsPath (OsPath, unsafeEncodeUtf)

import ShrinkVideos.Domain
    ( Verbosity (..)
    , defaultVerbosity
    )
import ShrinkVideos.Type.VideoFormat
    ( VideoFormat
    , defaultVideoFormat
    , parseVideoFormat
    , renderVideoFormat
    )

data Options = Options
    { optionsOutputFormat :: !VideoFormat
    , optionsRootPath :: !OsPath
    , optionsRecursive :: !Bool
    , optionsVerbosity :: !Verbosity
    }
    deriving (Eq, Show)

parseOptions :: IO Options
parseOptions = execParser parserInfo

parserInfo :: ParserInfo Options
parserInfo =
    info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Re-encode videos with x265 and keep only size-beneficial results."
            <> header "video-recoder"
        )

optionsParser :: Parser Options
optionsParser =
    Options
        <$> outputFormatParser
        <*> directoryParser
        <*> recursiveParser
        <*> verbosityParser

outputFormatParser :: Parser VideoFormat
outputFormatParser =
    option
        (eitherReader parseVideoFormat)
        ( short 'f'
            <> long "format"
            <> metavar "EXT"
            <> value defaultVideoFormat
            <> showDefaultWith renderVideoFormat
            <> help "Output container extension."
        )

directoryParser :: Parser OsPath
directoryParser =
    option
        (unsafeEncodeUtf <$> str)
        ( short 'd'
            <> long "directory"
            <> metavar "DIR"
            <> value (unsafeEncodeUtf ".")
            <> showDefaultWith (const ".")
            <> help "Directory to process without changing working directory."
        )

recursiveParser :: Parser Bool
recursiveParser =
    switch
        ( short 'r'
            <> long "recursive"
            <> help "Recursively process video files in subdirectories."
        )

verbosityParser :: Parser Verbosity
verbosityParser = chooseVerbosity <$> many verbosityFlag
  where
    chooseVerbosity [] = defaultVerbosity
    chooseVerbosity xs = last xs

verbosityFlag :: Parser Verbosity
verbosityFlag =
    asum
        [ flag' ExtraDetail (short 'x' <> help "Extra output")
        , flag' WarningsAndErrors (short 'w' <> help "Warnings and errors only")
        , flag' ErrorsOnly (short 'e' <> help "Errors only")
        , flag' Quiet (short 'q' <> help "No regular log output")
        ]
