{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ShrinkVideos.Logging
    ( Logger
    , buildLogger
    , logAt
    , renderIndexCounter
    , renderPaddedIndexCounter
    , renderPercentReduction
    ) where

import Control.Monad (when)
import Data.Colour.SRGB qualified as SRGB
import Data.Text (Text)
import Data.Word (Word8)
import Numeric (showFFloat)
import System.IO (Handle, stdout)

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Console.ANSI qualified as Ansi

import ShrinkVideos.Domain
    ( LogLevel (..)
    , Verbosity
    , shouldLog
    )

data Logger = Logger
    { loggerVerbosity :: !Verbosity
    , loggerTerminalHandle :: !Handle
    , loggerAnsiSupport :: !AnsiSupport
    }


data AnsiSupport
    = AnsiEnabled
    | AnsiDisabled


data HighlightKind
    = HighlightCounter
    | HighlightPositivePercent
    | HighlightNonPositivePercent


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
    supportsAnsi <- Ansi.hSupportsANSI loggerTerminalHandle
    let loggerAnsiSupport = case supportsAnsi of
            True -> AnsiEnabled
            False -> AnsiDisabled
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
logAt Logger{..} level message =
    when
        (shouldLog loggerVerbosity level)
        ( let renderedMessage = case loggerAnsiSupport of
                    AnsiEnabled -> renderAnsiLogEntry level message
                    AnsiDisabled -> renderPlainLogEntry level message
          in TIO.hPutStrLn loggerTerminalHandle renderedMessage
        )


{- |
@since 1.0.0
/O(\log_{10}(n))/
/O(\log_{10}(n))/

Renders an index counter segment such as @[  1 / 10 ]@ for inclusion in a log
message. The logger will render the segment in bold on ANSI-capable terminals.

<details>
<summary>Examples</summary>

@
>>> renderIndexCounter 1 10
"[  1 / 10 ]"
@
</details>
-}
renderIndexCounter :: Word -> Word -> Text
renderIndexCounter currentIndex totalCount =
    wrapHighlightedText HighlightCounter (renderVisibleIndexCounter currentIndex totalCount)


{- |
@since 1.0.0
/O(\log_{10}(n))/
/O(\log_{10}(n))/

Renders a fixed-width counter segment for aligned progress logging. The counter
and its trailing padding are emitted as a single bold segment.

<details>
<summary>Examples</summary>

@
>>> renderPaddedIndexCounter 1 10
"[  1 / 10 ]    "
@
</details>
-}
renderPaddedIndexCounter :: Word -> Word -> Text
renderPaddedIndexCounter currentIndex totalCount =
    let visibleCounter = renderVisibleIndexCounter currentIndex totalCount
        visibleCounterWidth = fromIntegral (T.length visibleCounter) :: Word
        paddingWidth =
            case counterDisplayWidth > visibleCounterWidth of
                True -> counterDisplayWidth - visibleCounterWidth
                False -> 0
    in wrapHighlightedText
        HighlightCounter
        (visibleCounter <> T.replicate (fromIntegral paddingWidth) " ")


{- |
@since 1.0.0
/O(1)/
/O(1)/

Renders a percentage-reduction segment such as @85.112%@ for inclusion in a log
message. Positive values are bold green; zero and negative values are bold orange.

<details>
<summary>Examples</summary>

@
>>> renderPercentReduction 85.112
"85.112%"
>>> renderPercentReduction 0
"0.000%"
@
</details>
-}
renderPercentReduction :: Double -> Text
renderPercentReduction reductionPercent =
    let highlightKind = case reductionPercent > 0 of
            True -> HighlightPositivePercent
            False -> HighlightNonPositivePercent
    in wrapHighlightedText highlightKind (formatPercentage reductionPercent)


renderPlainLogEntry :: LogLevel -> Text -> Text
renderPlainLogEntry level message =
    renderLevelPrefix level <> stripHighlightMarkup message


renderAnsiLogEntry :: LogLevel -> Text -> Text
renderAnsiLogEntry level message =
    renderLevelStyle level
        <> renderLevelPrefix level
        <> renderAnsiMarkup level message
        <> renderSgrCode [Ansi.Reset]


renderAnsiMarkup :: LogLevel -> Text -> Text
renderAnsiMarkup level message =
    case T.uncons message of
        Nothing -> ""
        Just (nextCharacter, _)
            | nextCharacter == counterOpenMarker ->
                renderHighlightedSegment level HighlightCounter (T.tail message)
            | nextCharacter == positivePercentOpenMarker ->
                renderHighlightedSegment level HighlightPositivePercent (T.tail message)
            | nextCharacter == nonPositivePercentOpenMarker ->
                renderHighlightedSegment level HighlightNonPositivePercent (T.tail message)
            | nextCharacter == closeMarker ->
                renderLevelStyle level <> renderAnsiMarkup level (T.tail message)
            | otherwise ->
                let (plainText, remainingText) = T.span (not . isMarkupCharacter) message
                in plainText <> renderAnsiMarkup level remainingText


renderHighlightedSegment :: LogLevel -> HighlightKind -> Text -> Text
renderHighlightedSegment level highlightKind message =
    let (highlightedText, remainder) = T.break (== closeMarker) message
    in case T.uncons remainder of
        Nothing ->
            renderHighlightStyle highlightKind
                <> highlightedText
        Just (_, remainingMessage) ->
            renderHighlightStyle highlightKind
                <> highlightedText
                <> renderLevelStyle level
                <> renderAnsiMarkup level remainingMessage


wrapHighlightedText :: HighlightKind -> Text -> Text
wrapHighlightedText highlightKind highlightedText =
    let openMarker = case highlightKind of
            HighlightCounter -> counterOpenMarker
            HighlightPositivePercent -> positivePercentOpenMarker
            HighlightNonPositivePercent -> nonPositivePercentOpenMarker
    in T.cons openMarker (highlightedText <> T.singleton closeMarker)


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


stripHighlightMarkup :: Text -> Text
stripHighlightMarkup =
    T.filter (not . isMarkupCharacter)


isMarkupCharacter :: Char -> Bool
isMarkupCharacter character =
    character == counterOpenMarker
        || character == positivePercentOpenMarker
        || character == nonPositivePercentOpenMarker
        || character == closeMarker


renderLevelPrefix :: LogLevel -> Text
renderLevelPrefix = \case
    LevelError -> "[X] "
    LevelWarning -> "[!] "
    LevelInfo -> "    "
    LevelExtra -> "[+] "


renderLevelStyle :: LogLevel -> Text
renderLevelStyle level =
    renderSgrCode
        [ Ansi.SetColor Ansi.Foreground Ansi.Vivid (levelColor level)
        ]


levelColor :: LogLevel -> Ansi.Color
levelColor = \case
    LevelError -> Ansi.Red
    LevelWarning -> Ansi.Yellow
    LevelInfo -> Ansi.White
    LevelExtra -> Ansi.Cyan


renderHighlightStyle :: HighlightKind -> Text
renderHighlightStyle highlightKind =
    case highlightKind of
        HighlightCounter ->
            renderSgrCode
                [ Ansi.SetConsoleIntensity Ansi.BoldIntensity
                ]
        HighlightPositivePercent ->
            renderSgrCode
                [ Ansi.SetConsoleIntensity Ansi.BoldIntensity
                , Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Green
                ]
        HighlightNonPositivePercent ->
            renderSgrCode
                [ Ansi.SetConsoleIntensity Ansi.BoldIntensity
                , Ansi.SetRGBColor Ansi.Foreground (SRGB.sRGB24 orangeRed orangeGreen orangeBlue)
                ]


renderSgrCode :: [Ansi.SGR] -> Text
renderSgrCode =
    T.pack . Ansi.setSGRCode


counterDisplayWidth :: Word
counterDisplayWidth = 15


counterOpenMarker :: Char
counterOpenMarker = '\xE000'


positivePercentOpenMarker :: Char
positivePercentOpenMarker = '\xE001'


nonPositivePercentOpenMarker :: Char
nonPositivePercentOpenMarker = '\xE002'


closeMarker :: Char
closeMarker = '\xE003'


orangeRed :: Word8
orangeRed = 255


orangeGreen :: Word8
orangeGreen = 165


orangeBlue :: Word8
orangeBlue = 0
