{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ShrinkVideos.Logging
    ( Logger
    , LogMessage
    , buildLogger
    , logAt
    , logStyledAt
    , plain
    , renderIndexCounter
    , renderPaddedIndexCounter
    , renderPercentReduction
    ) where

import Control.Monad (forM_, when)
import Data.String (IsString (..))
import Data.Text (Text)
import Numeric (showFFloat)
import System.IO (Handle, stdout)

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Console.ANSI qualified as ANSI
import System.Console.ANSI.Types qualified as ANSITypes

import ShrinkVideos.Domain
    ( LogLevel (..)
    , Verbosity
    , shouldLog
    )

data Logger = Logger
    { loggerVerbosity :: !Verbosity
    , loggerTerminalHandle :: !Handle
    , loggerANSISupport :: !ANSISupport
    }


data ANSISupport
    = ANSIEnabled
    | ANSIDisabled


data HighlightKind
    = HighlightCounter
    | HighlightPositivePercent
    | HighlightNonPositivePercent


newtype LogMessage = LogMessage
    { logMessageFragments :: [LogFragment]
    }


data LogFragment
    = PlainText !Text
    | HighlightedText !HighlightKind !Text


instance Semigroup LogMessage where
    LogMessage leftFragments <> LogMessage rightFragments =
        LogMessage (leftFragments <> rightFragments)


instance Monoid LogMessage where
    mempty = LogMessage []


instance IsString LogMessage where
    fromString =
        plain . T.pack


{- |
@since 1.0.0
/O(1)/
/O(1)/

Constructs a terminal logger that honors the requested verbosity and enables
ANSI styling only when the terminal supports it.

<details>
<summary>Examples</summary>

@
>>> buildLogger Informational
-- Logger configured for stdout output
@
</details>
-}
buildLogger :: Verbosity -> IO Logger
buildLogger loggerVerbosity = do
    let loggerTerminalHandle = stdout
    supportsANSI <- ANSI.hNowSupportsANSI loggerTerminalHandle
    let loggerANSISupport = case supportsANSI of
            True -> ANSIEnabled
            False -> ANSIDisabled
    pure Logger{..}


{- |
@since 1.0.0
/O(n)/ in the length of the rendered log message.
/O(n)/ in the length of the rendered log message.

Emits a log entry using the requested level. Terminal output is colorized with
ANSI escape sequences when supported, while redirected output remains plain.

<details>
<summary>Examples</summary>

@
>>> logAt logger LevelWarning "Disk nearly full"
[!] Disk nearly full
@
</details>
-}
logAt :: Logger -> LogLevel -> Text -> IO ()
logAt logger level =
    logStyledAt logger level . plain


{- |
@since 1.0.0
/O(n)/ in the length of the rendered log message.
/O(n)/ in the length of the rendered log message.

Emits a structured log entry using the requested level, styling highlighted
fragments through the @ansi-terminal@ API when supported.
-}
logStyledAt :: Logger -> LogLevel -> LogMessage -> IO ()
logStyledAt Logger{..} level message =
    when
        (shouldLog loggerVerbosity level)
        $ case loggerANSISupport of
            ANSIEnabled -> writeANSILogEntry loggerTerminalHandle level message
            ANSIDisabled -> TIO.hPutStrLn loggerTerminalHandle (renderPlainLogEntry level message)


plain :: Text -> LogMessage
plain text
    | T.null text = mempty
    | otherwise = LogMessage [PlainText text]


{- |
@since 1.0.0
/O(\log_{10}(n))/
/O(\log_{10}(n))/

Renders an index counter segment such as @[  1 / 10 ]@ for inclusion in a log
message. The logger will render the segment in bold on ANSI-capable terminals.

<details>
<summary>Examples</summary>

@
renderIndexCounter 1 10
-- renders as "[  1 / 10 ]"
@
</details>
-}
renderIndexCounter :: Word -> Word -> LogMessage
renderIndexCounter currentIndex totalCount =
    highlighted HighlightCounter (renderVisibleIndexCounter currentIndex totalCount)


{- |
@since 1.0.0
/O(\log_{10}(n))/
/O(\log_{10}(n))/

Renders a fixed-width counter segment for aligned progress logging. The counter
and its trailing padding are emitted as a single bold segment.

<details>
<summary>Examples</summary>

@
renderPaddedIndexCounter 1 10
-- renders as "[  1 / 10 ]    "
@
</details>
-}
renderPaddedIndexCounter :: Word -> Word -> LogMessage
renderPaddedIndexCounter currentIndex totalCount =
    let visibleCounter = renderVisibleIndexCounter currentIndex totalCount
        visibleCounterWidth = fromIntegral (T.length visibleCounter) :: Word
        paddingWidth =
            case counterDisplayWidth > visibleCounterWidth of
                True -> counterDisplayWidth - visibleCounterWidth
                False -> 0
    in highlighted
        HighlightCounter
        (visibleCounter <> T.replicate (fromIntegral paddingWidth) " ")


{- |
@since 1.0.0
/O(1)/
/O(1)/

Renders a percentage-reduction segment such as @85.112%@ for inclusion in a log
message. Positive values are bold green; zero and negative values are bold
magenta.

<details>
<summary>Examples</summary>

@
renderPercentReduction 85.112
-- renders as "85.112%"
renderPercentReduction 0
-- renders as "0.000%"
@
</details>
-}
renderPercentReduction :: Double -> LogMessage
renderPercentReduction reductionPercent =
    let highlightKind = case reductionPercent > 0 of
            True -> HighlightPositivePercent
            False -> HighlightNonPositivePercent
    in highlighted highlightKind (formatPercentage reductionPercent)


highlighted :: HighlightKind -> Text -> LogMessage
highlighted highlightKind text
    | T.null text = mempty
    | otherwise = LogMessage [HighlightedText highlightKind text]


renderPlainLogEntry :: LogLevel -> LogMessage -> Text
renderPlainLogEntry level message =
    renderLevelPrefix level <> flattenLogMessage message


writeANSILogEntry :: Handle -> LogLevel -> LogMessage -> IO ()
writeANSILogEntry terminalHandle level message = do
    writeStyledText terminalHandle (levelStyle level) (renderLevelPrefix level)
    forM_ (logMessageFragments message) (writeStyledFragment terminalHandle level)
    ANSI.hSetSGR terminalHandle [ANSITypes.Reset]
    TIO.hPutStr terminalHandle "\n"


writeStyledFragment :: Handle -> LogLevel -> LogFragment -> IO ()
writeStyledFragment terminalHandle level = \case
    PlainText text ->
        writeStyledText terminalHandle (levelStyle level) text
    HighlightedText highlightKind text ->
        writeStyledText terminalHandle (highlightStyle level highlightKind) text


writeStyledText :: Handle -> [ANSITypes.SGR] -> Text -> IO ()
writeStyledText _ _ text
    | T.null text = pure ()
writeStyledText terminalHandle sgrs text = do
    ANSI.hSetSGR terminalHandle (ANSITypes.Reset : sgrs)
    TIO.hPutStr terminalHandle text


flattenLogMessage :: LogMessage -> Text
flattenLogMessage =
    foldMap renderLogFragmentText . logMessageFragments


renderLogFragmentText :: LogFragment -> Text
renderLogFragmentText = \case
    PlainText text -> text
    HighlightedText _ text -> text


renderVisibleIndexCounter :: Word -> Word -> Text
renderVisibleIndexCounter currentIndex totalCount =
    let currentIndexText = T.pack (show currentIndex)
        totalCountText = T.pack (show totalCount)
        sharedWidth = max (T.length currentIndexText) (T.length totalCountText)
        paddedCurrentIndexText = leftPad sharedWidth currentIndexText
        paddedTotalCountText = leftPad sharedWidth totalCountText
    in T.unwords ["[", paddedCurrentIndexText, "/", paddedTotalCountText, "]"]


leftPad :: Int -> Text -> Text
leftPad targetWidth value =
    let paddingWidth = max 0 (targetWidth - T.length value)
    in T.replicate paddingWidth " " <> value


formatPercentage :: Double -> Text
formatPercentage reductionPercent =
    T.pack (showFFloat (Just 3) reductionPercent "" <> "%")


renderLevelPrefix :: LogLevel -> Text
renderLevelPrefix = \case
    LevelError -> "[X] "
    LevelWarning -> "[!] "
    LevelInfo -> "    "
    LevelExtra -> "[+] "


levelStyle :: LogLevel -> [ANSITypes.SGR]
levelStyle level =
    [ ANSITypes.SetColor ANSITypes.Foreground ANSITypes.Vivid (levelColor level)
    ]


levelColor :: LogLevel -> ANSITypes.Color
levelColor = \case
    LevelError -> ANSITypes.Red
    LevelWarning -> ANSITypes.Yellow
    LevelInfo -> ANSITypes.White
    LevelExtra -> ANSITypes.Cyan


highlightStyle :: LogLevel -> HighlightKind -> [ANSITypes.SGR]
highlightStyle level highlightKind =
    case highlightKind of
        HighlightCounter ->
            levelStyle level
                <> [ ANSITypes.SetConsoleIntensity ANSITypes.BoldIntensity
                   ]
        HighlightPositivePercent ->
            [ ANSITypes.SetConsoleIntensity ANSITypes.BoldIntensity
            , ANSITypes.SetColor ANSITypes.Foreground ANSITypes.Vivid ANSITypes.Green
            ]
        HighlightNonPositivePercent ->
            [ ANSITypes.SetConsoleIntensity ANSITypes.BoldIntensity
            , ANSITypes.SetColor ANSITypes.Foreground ANSITypes.Vivid ANSITypes.Magenta
            ]


counterDisplayWidth :: Word
counterDisplayWidth = 15
