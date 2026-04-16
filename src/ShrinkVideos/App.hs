{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ShrinkVideos.App
    ( RuntimeEnv
    , buildRuntimeEnv
    , runShrinkVideos
    , cancelCurrentFfmpeg
    , cleanupAndReport
    ) where

import Control.Concurrent.Async (Async, async, cancel, wait)
import Control.Exception (IOException, bracket, evaluate, finally, throwIO, try)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty(..), sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Numeric (showFFloat)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory.OsPath
    ( doesDirectoryExist
    , doesFileExist
    , getFileSize
    , getTemporaryDirectory
    , listDirectory
    , removeFile
    , renamePath
    )
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hGetContents, openTempFile)
import System.Process
    ( CreateProcess (std_err, std_out)
    , ProcessHandle
    , StdStream (CreatePipe)
    , createProcess
    , proc
    , terminateProcess
    , waitForProcess
    )
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone, utcToLocalTime)

import Colog (LogAction, logTextStdout, unLogAction)

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.OsPath qualified as Path

import ShrinkVideos.Cli (Options (..))
import ShrinkVideos.Domain
    ( LogLevel (..)
    , RecodeDecision (..)
    , RecodeResult (..)
    , Summary (..)
    , Verbosity
    , addRecodeResult
    , decideRecodeDecision
    , emptySummary
    , isX265Codec
    , setSummaryFilesFound
    , shouldLog
    )
import ShrinkVideos.Type.FileSize
    ( FileSize
    , fileSizeFromIntegerMaybe
    , renderFileSizeSI
    )
import ShrinkVideos.Type.EncodingOutcome
    ( EncodingOutcome (..)
    )
import ShrinkVideos.Type.VideoFile
    ( VideoFile (..)
    )
import ShrinkVideos.Type.VideoFormat
    ( VideoFormat
    , ffmpegFormat
    , isVideoFile
    , toOsPath
    )

data RuntimeEnv = RuntimeEnv
    { runtimeOptions :: !Options
    , runtimeLogger :: !Logger
    , runtimeStartedAt :: !UTCTime
    , runtimeTempDirectory :: !Path.OsPath
    , runtimeCurrentTempFile :: !(IORef (Maybe Path.OsPath))
    , runtimeCurrentFfmpegWorker :: !(IORef (Maybe (Async CommandResult)))
    , runtimeSummary :: !(IORef Summary)
    }

data Logger = Logger
    { loggerVerbosity :: !Verbosity
    , loggerSink :: !(LogAction IO Text)
    }

newtype AppM a = AppM { unAppM :: ReaderT RuntimeEnv IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader RuntimeEnv)

newtype CandidateVideo = CandidateVideo
    { candidateVideoFile :: VideoFile
    }
    deriving (Eq, Show)

candidateVideoPath :: CandidateVideo -> Path.OsPath
candidateVideoPath = videoFilePath . candidateVideoFile

candidateVideoSize :: CandidateVideo -> FileSize
candidateVideoSize = videoFileSize . candidateVideoFile

data CommandResult = CommandResult
    { commandExitCode :: !ExitCode
    , commandStdout :: !Text
    , commandStderr :: !Text
    }

data ProbeResult
    = ProbeFailed
    | NoVideoStreams
    | VideoCodecs !(NonEmpty Text)

buildRuntimeEnv :: Options -> IO RuntimeEnv
buildRuntimeEnv runtimeOptions = do
    runtimeStartedAt <- getCurrentTime
    runtimeTempDirectory <- getTemporaryDirectory
    runtimeCurrentTempFile <- newIORef Nothing
    runtimeCurrentFfmpegWorker <- newIORef Nothing
    runtimeSummary <- newIORef emptySummary
    let runtimeLogger =
            Logger
                { loggerVerbosity = optionsVerbosity runtimeOptions
                , loggerSink = logTextStdout
                }
    pure RuntimeEnv{..}

runShrinkVideos :: RuntimeEnv -> IO ()
runShrinkVideos env = runReaderT (unAppM runProgram) env

cancelCurrentFfmpeg :: RuntimeEnv -> IO ()
cancelCurrentFfmpeg env = do
    currentWorker <- atomicModifyIORef' (runtimeCurrentFfmpegWorker env) (\worker -> (Nothing, worker))
    case currentWorker of
        Nothing -> pure ()
        Just worker -> cancel worker

cleanupAndReport :: RuntimeEnv -> IO ()
cleanupAndReport env = do
    cancelCurrentFfmpeg env
    runReaderT (unAppM removeCurrentTempFile) env
    runtimeStoppedAt <- getCurrentTime
    summary <- readIORef (runtimeSummary env)
    reportSummary (runtimeStartedAt env) runtimeStoppedAt summary

runProgram :: AppM ()
runProgram = do
    RuntimeEnv{runtimeOptions = Options{..}, ..} <- ask
    runtimeTempDirectoryText <- pathToText runtimeTempDirectory
    logExtra ("Using temporary directory: " <> runtimeTempDirectoryText)

    rootIsDirectory <- safeDoesDirectoryExist optionsRootPath
    if not rootIsDirectory
        then do
            rootPathText <- pathToText optionsRootPath
            logWarning ("Directory does not exist or is not a directory: " <> rootPathText)
        else do
            videoFiles <- collectVideoFiles optionsRootPath optionsRecursive
            (x265Files, candidates) <- partitionCandidates videoFiles
            liftIO $
                modifyIORef' runtimeSummary $
                    setSummaryFilesFound (toWordCount (length candidates))

            let renderFoundMetrics suffix xs =
                  let count = tshow . toWordCount $ length xs
                      space = T.pack . renderFileSizeSI . sum $ videoFileSize <$> xs
                  in  logInfo $ T.unwords [ "  -", count, "totaling", space, suffix]
            logInfo $ "Recognized " <> tshow (toWordCount (length videoFiles)) <> " video files:"
            renderFoundMetrics "identified as x265/hevc encoded" x265Files
            renderFoundMetrics "designated to be re-encoded" $ candidateVideoFile <$> candidates
            case candidates of
                [] -> logInfo "No re-encoding candidate video files found!"
                x:xs -> do
                    let sortedCandidates = sortOn (Down . candidateVideoSize) $ x:|xs
                        candidateCount   = toWordCount $ length sortedCandidates
                    forM_ (NE.zip ((1 :: Word) :| [ 2 ..]) sortedCandidates) $ \(currentIndex, candidate) ->
                        processCandidate currentIndex candidateCount candidate

collectVideoFiles :: Path.OsPath -> Bool -> AppM [VideoFile]
collectVideoFiles rootPath shouldRecurse = walk rootPath
  where
    walk currentPath = do
        entriesResult <- liftIO (try (listDirectory currentPath) :: IO (Either IOException [Path.OsPath]))
        case entriesResult of
            Left ioEx -> do
                currentPathText <- displayPathFromRoot rootPath currentPath
                logWarning $
                    "Failed to read directory '"
                        <> currentPathText
                        <> "': "
                        <> T.pack (show ioEx)
                pure []
            Right entries -> fmap concat . forM entries $ \entry -> do
                let childPath = currentPath Path.</> entry
                isDirectory <- safeDoesDirectoryExist childPath
                if isDirectory
                    then
                        if shouldRecurse
                            then walk childPath
                            else pure []
                    else do
                        isFile <- safeDoesFileExist childPath
                        if isFile
                            then
                                case isVideoFile childPath of
                                    Nothing -> pure []
                                    Just videoFormat -> do
                                        videoFile <- collectVideoFile rootPath childPath videoFormat
                                        case videoFile of
                                            Nothing -> pure []
                                            Just resolvedVideoFile -> pure [resolvedVideoFile]
                            else pure []

collectVideoFile :: Path.OsPath -> Path.OsPath -> VideoFormat -> AppM (Maybe VideoFile)
collectVideoFile rootPath inputPath videoFormat = do
    sizeResult <- liftIO (try (getFileSize inputPath) :: IO (Either IOException Integer))
    inputPathText <- displayPathFromRoot rootPath inputPath
    case sizeResult of
        Left ioEx -> do
            logWarning $
                "Skipping '"
                    <> inputPathText
                    <> "' because size metadata lookup failed: "
                    <> T.pack (show ioEx)
            pure Nothing
        Right fileSizeInteger ->
            case fileSizeFromIntegerMaybe fileSizeInteger of
                Nothing -> do
                    logWarning $
                        "Skipping '"
                            <> inputPathText
                            <> "' because file size was negative."
                    pure Nothing
                Just resolvedFileSize ->
                    pure $
                        Just
                            VideoFile
                                { videoFilePath = inputPath
                                , videoFileFormat = videoFormat
                                , videoFileSize = resolvedFileSize
                                }

partitionCandidates :: [VideoFile] -> AppM ([VideoFile], [CandidateVideo])
partitionCandidates inputFiles = foldr determineCandidacy  (pure ([], [])) inputFiles

determineCandidacy :: VideoFile -> AppM ([VideoFile], [CandidateVideo]) -> AppM ([VideoFile], [CandidateVideo])
determineCandidacy inputFile currResult = do
    (x265Files, candidateFiles) <- currResult
    candidateResult <- collectCandidate inputFile
    pure $
        case candidateResult of
            Nothing -> (x265Files, candidateFiles)
            Just e -> case e of
              Left encoded -> (encoded : x265Files, candidateFiles)
              Right candidate -> (x265Files, candidate : candidateFiles)

collectCandidate :: VideoFile -> AppM (Maybe (Either VideoFile  CandidateVideo))
collectCandidate inputFile@VideoFile{..} = do
    rootPath <- asks (optionsRootPath . runtimeOptions)
    probeResult <- probeVideoCodecs videoFilePath
    inputPathText <- displayPathFromRoot rootPath videoFilePath
    case probeResult of
        ProbeFailed -> do
            logWarning ("Skipping '" <> inputPathText <> "' because codec probing failed.")
            pure Nothing
        NoVideoStreams -> do
            logWarning ("Skipping '" <> inputPathText <> "' because no video stream was detected.")
            pure Nothing
        VideoCodecs codecNames
            | all isX265Codec codecNames -> do
                logExtra $
                    "Skipping already x265/hevc file: "
                        <> inputPathText
                pure . Just $ Left inputFile
            | otherwise ->
                pure . Just . Right $ CandidateVideo inputFile

probeVideoCodecs :: Path.OsPath -> AppM ProbeResult
probeVideoCodecs inputPath = do
    pathStr <- osPathToString inputPath
    CommandResult{..} <-
        runCommandCapture
            ( "ffprobe")
            [ "-v"
            , "error"
            , "-select_streams"
            , "v"
            , "-show_entries"
            , "stream=codec_name"
            , "-of"
            , "default=nw=1:nk=1"
            , pathStr
            ]
    case commandExitCode of
        ExitSuccess ->
            case NE.nonEmpty (filter (not . T.null) (T.strip <$> T.lines commandStdout)) of
                Nothing -> pure NoVideoStreams
                Just codecNames -> pure (VideoCodecs codecNames)
        ExitFailure _ -> do
            unless (T.null commandStderr) $
                logWarning ("ffprobe error output: " <> T.strip commandStderr)
            pure ProbeFailed

processCandidate :: Word -> Word -> CandidateVideo -> AppM ()
processCandidate currentIndex totalCount candidate =
    withTemporaryFile $ \tempPath -> do
        Options{..} <- asks runtimeOptions
        localZone <- liftIO getCurrentTimeZone
        startedText <- liftIO (renderLocalTimestamp localZone =<< getCurrentTime)
        fileLabel <- pathToText (Path.takeFileName (candidateVideoPath candidate))
        let renderCounterFixedWidth i t =
              let counterStr = unwords [ "[", show i, "/", show t, "]" ]
                  len = length counterStr
              in  T.pack $ counterStr <> replicate (15 - len) ' '

        logInfo $ mconcat
            [ startedText
            , " @ "
            , renderCounterFixedWidth currentIndex totalCount
            , "\t"
            , fileLabel
            ]

        encodeResult <-
            ffmpegEncodeX265
                optionsOutputFormat
                (candidateVideoSize candidate)
                (candidateVideoPath candidate)
                tempPath
        case encodeResult of
            Left ffmpegError ->
                logWarning $
                    "["
                        <> tshow currentIndex
                        <> "/"
                        <> tshow totalCount
                        <> "] Failed x265 encode for "
                        <> fileLabel
                        <> ": "
                        <> ffmpegError
            Right outcome ->
                case decideRecodeDecision
                    (encodingOutcomeOriginalFileSize outcome)
                    (encodingOutcomeFinishedFileSize outcome) of
                    KeepEncodedOutput -> do
                        finalSize <- replaceOriginalWithTemp candidate tempPath
                        stoppedText <- liftIO (renderLocalTimestamp localZone (encodingOutcomeStoppedAt outcome))
                        let savedPercent =
                                if candidateVideoSize candidate == 0
                                    then 0
                                    else ((fromIntegral (candidateVideoSize candidate) - fromIntegral finalSize) / fromIntegral (candidateVideoSize candidate) :: Double) * 100
                            elapsedSeconds =
                                max 0 (ceiling (diffUTCTime (encodingOutcomeStoppedAt outcome) (encodingOutcomeStartedAt outcome)) :: Integer)
                        logInfo $
                            T.concat
                                [ stoppedText
                                , " ~ "
                                , renderDetailedElapsed elapsedSeconds
                                , "\t"
                                , T.pack (renderPercentage3 savedPercent)
                                , " "
                                , T.pack (renderFileSizeSI finalSize)
                                , " / "
                                , T.pack (renderFileSizeSI (candidateVideoSize candidate))
                                ]
                    ReencodeCopyForExtension -> do
                        copyResult <-
                            ffmpegCopyForExtension
                                optionsOutputFormat
                                (candidateVideoSize candidate)
                                (candidateVideoPath candidate)
                                tempPath
                        case copyResult of
                            Left copyError ->
                                logWarning $
                                    "["
                                        <> tshow currentIndex
                                        <> "/"
                                        <> tshow totalCount
                                        <> "] Copy-for-extension failed for "
                                        <> fileLabel
                                        <> ": "
                                        <> copyError
                            Right copyOutcome -> do
                                finalSize <- replaceOriginalWithTemp candidate tempPath
                                logInfo $
                                    "Used copy-for-extension for "
                                        <> fileLabel
                                        <> " ("
                                        <> tshow (encodingOutcomeOriginalFileSize copyOutcome)
                                        <> " -> "
                                        <> tshow finalSize
                                        <> " bytes)."

ffmpegEncodeX265 :: VideoFormat -> FileSize -> Path.OsPath -> Path.OsPath -> AppM (Either Text EncodingOutcome)
ffmpegEncodeX265 outputFormat originalFileSize inputPath tempPath = do
    startedAt <- liftIO getCurrentTime
    ffmpegFlags <- ffmpegStandardizedFlags
        outputFormat
        inputPath
        tempPath
        [ "-c:v"
        , "libx265"
        , "-x265-params"
        , "log-level=error"
        , "-crf"
        , "25"
        , "-max_muxing_queue_size"
        , "4096"
        , "-preset"
        , "slower"
        , "-vf"
        , "crop=trunc(iw/2)*2:trunc(ih/2)*2"
        ]
    CommandResult{..} <- runCommandCapture "ffmpeg" ffmpegFlags
    stoppedAt <- liftIO getCurrentTime
    finishedFileSize <- tempFileSize tempPath
    pure $
        case commandExitCode of
            ExitSuccess ->
                case finishedFileSize of
                    Nothing ->
                        Left "ffmpeg completed but the temporary output size could not be read."
                    Just size ->
                        Right EncodingOutcome
                            { encodingOutcomeOriginalFileSize = originalFileSize
                            , encodingOutcomeFinishedFileSize = size
                            , encodingOutcomeStartedAt = startedAt
                            , encodingOutcomeStoppedAt = stoppedAt
                            }
            ExitFailure _ ->
                Left (if T.null commandStderr then "ffmpeg encode failed." else T.strip commandStderr)

ffmpegCopyForExtension :: VideoFormat -> FileSize -> Path.OsPath -> Path.OsPath -> AppM (Either Text EncodingOutcome)
ffmpegCopyForExtension outputFormat originalFileSize inputPath tempPath = do
    startedAt <- liftIO getCurrentTime
    ffmpegFlags <- ffmpegStandardizedFlags
        outputFormat
        inputPath
        tempPath
        [ "-c"
        , "copy"
        ]
    CommandResult{..} <- runCommandCapture "ffmpeg" ffmpegFlags
    stoppedAt <- liftIO getCurrentTime
    finishedFileSize <- tempFileSize tempPath
    pure $
        case commandExitCode of
            ExitSuccess ->
                case finishedFileSize of
                    Nothing ->
                        Left "ffmpeg completed but the temporary output size could not be read."
                    Just size ->
                        Right EncodingOutcome
                            { encodingOutcomeOriginalFileSize = originalFileSize
                            , encodingOutcomeFinishedFileSize = size
                            , encodingOutcomeStartedAt = startedAt
                            , encodingOutcomeStoppedAt = stoppedAt
                            }
            ExitFailure _ ->
                Left (if T.null commandStderr then "ffmpeg copy failed." else T.strip commandStderr)

replaceOriginalWithTemp :: CandidateVideo -> Path.OsPath -> AppM FileSize
replaceOriginalWithTemp candidate tempPath = do
    RuntimeEnv{runtimeOptions = Options{..}, runtimeSummary = summaryRef} <- ask
    let finalPath = Path.replaceExtension (candidateVideoPath candidate) (toOsPath optionsOutputFormat)
    finalPathText <- displayPathFromRoot optionsRootPath finalPath

    liftIO $ do
        when (finalPath /= candidateVideoPath candidate) (removeFileIfExists finalPath)
        removeFileIfExists (candidateVideoPath candidate)
        renamePath tempPath finalPath

    finalSizeResult <- liftIO (try (getFileSize finalPath) :: IO (Either IOException Integer))
    case finalSizeResult of
        Left ioEx -> do
            logWarning $
                "Unable to read final size for '"
                    <> finalPathText
                    <> "': "
                    <> T.pack (show ioEx)
            pure (candidateVideoSize candidate)
        Right finalSizeInteger ->
            case fileSizeFromIntegerMaybe finalSizeInteger of
                Nothing -> do
                    logWarning $
                        "Unable to use negative final size for '"
                            <> finalPathText
                            <> "'."
                    pure (candidateVideoSize candidate)
                Just finalSize -> do
                    liftIO $
                        modifyIORef' summaryRef $
                            addRecodeResult
                                RecodeResult
                                    { recodeResultOriginalBytes = candidateVideoSize candidate
                                    , recodeResultFinalBytes = finalSize
                                    }
                    pure finalSize

ffmpegStandardizedFlags :: VideoFormat -> Path.OsPath -> Path.OsPath -> [String] -> AppM [String]
ffmpegStandardizedFlags outputFormat inputPath tempPath flags = do
    strPathInput  <- osPathToString inputPath
    strPathOutput <- osPathToString tempPath
    pure $
        [ "-y"
        , "-hide_banner"
        , "-loglevel"
        , "error"
        , "-nostats"
        , "-i"
        , strPathInput
        , "-b:v"
        , "0"
        , "-f"
        , ffmpegFormat outputFormat
        ] <> flags <> [ strPathOutput ]

runCommandCapture :: String -> [String] -> AppM CommandResult
runCommandCapture executable args = do
    let action = runProcessCapture executable args
    if  executable == "ffmpeg"
    then do
            workerRef <- asks runtimeCurrentFfmpegWorker
            worker <- liftIO $ async action
            liftIO (writeIORef workerRef (Just worker))
            liftIO
                (wait worker `finally` writeIORef workerRef Nothing)
    else liftIO $ action

withTemporaryFile :: (Path.OsPath -> AppM a) -> AppM a
withTemporaryFile action = do
    env@RuntimeEnv{runtimeOptions = Options{..}, ..} <- ask
    let createTemp = do
            runtimeTempDirectoryText <- Path.decodeUtf runtimeTempDirectory
            (tempBasePath, tempHandle) <- openTempFile runtimeTempDirectoryText "video-recoder"
            hClose tempHandle
            tempBasePathOs <- Path.encodeUtf tempBasePath
            let tempPath = Path.addExtension tempBasePathOs (toOsPath optionsOutputFormat)
            renamePath tempBasePathOs tempPath
            writeIORef runtimeCurrentTempFile (Just tempPath)
            pure tempPath

        cleanupTemp tempPath = do
            writeIORef runtimeCurrentTempFile Nothing
            removeFileIfExists tempPath
    AppM . ReaderT $ \_ ->
        bracket
            createTemp
            cleanupTemp
            (\tempPath -> runReaderT (unAppM (action tempPath)) env)

removeCurrentTempFile :: AppM ()
removeCurrentTempFile = do
    tempRef <- asks runtimeCurrentTempFile
    currentTemp <- liftIO (atomicModifyIORef' tempRef (\x -> (Nothing, x)))
    case currentTemp of
        Nothing -> pure ()
        Just tempPath -> liftIO (removeFileIfExists tempPath)

reportSummary :: UTCTime -> UTCTime -> Summary -> IO ()
reportSummary startedAt stoppedAt Summary{..} = do
    localZone <- getCurrentTimeZone
    startedText <- renderLocalTimestamp localZone startedAt
    stoppedText <- renderLocalTimestamp localZone stoppedAt
    let savedBytes
            | summaryFinalBytes > summaryOriginalBytes = 0
            | otherwise = summaryOriginalBytes - summaryFinalBytes
        elapsedSeconds = max 0 (ceiling (diffUTCTime stoppedAt startedAt) :: Integer)
        ratioPercent =
            let originalBytesInteger = toInteger summaryOriginalBytes
            in  if originalBytesInteger == 0
                then 0
                else 100 * (fromIntegral summaryFinalBytes / fromIntegral summaryOriginalBytes :: Double)
    TIO.putStrLn $
        "Files re-encoded: "
            <> tshow summaryFilesReencoded
            <> " / "
            <> tshow summaryFilesFound
    TIO.putStrLn "Disk space:"
    TIO.putStrLn $ "  - Original:\t" <> T.pack (renderFileSizeSI summaryOriginalBytes)
    TIO.putStrLn $ "  - Recoded: \t" <> T.pack (renderFileSizeSI summaryFinalBytes)
    TIO.putStrLn $ "  - Saved:   \t" <> T.pack (renderFileSizeSI savedBytes)
    TIO.putStrLn $ "  - Ratio:   \t" <> T.pack (renderPercentage3 ratioPercent)
    TIO.putStrLn "Timing:"
    TIO.putStrLn $ "  - Started:\t" <> startedText
    TIO.putStrLn $ "  - Stopped:\t" <> stoppedText
    TIO.putStrLn $ "  - Elapsed:\t" <> renderDetailedElapsed elapsedSeconds
    TIO.putStrLn $ "  - Saved/Sec:\t" <> T.pack (renderSignedRateSI savedBytes elapsedSeconds)

logWarning :: Text -> AppM ()
logWarning = logAt LevelWarning

logInfo :: Text -> AppM ()
logInfo = logAt LevelInfo

logExtra :: Text -> AppM ()
logExtra = logAt LevelExtra

logAt :: LogLevel -> Text -> AppM ()
logAt level message = do
    Logger{..} <- asks runtimeLogger
    when (shouldLog loggerVerbosity level) $
        liftIO $
            unLogAction loggerSink (prefixForLevel level <> message)
  where
    prefixForLevel LevelError = "[X] "
    prefixForLevel LevelWarning = "[!] "
    prefixForLevel LevelInfo = "    "
    prefixForLevel LevelExtra = "[+] "

safeDoesDirectoryExist :: Path.OsPath -> AppM Bool
safeDoesDirectoryExist path = do
    result <- liftIO (try (doesDirectoryExist path) :: IO (Either IOException Bool))
    pure (either (const False) id result)

safeDoesFileExist :: Path.OsPath -> AppM Bool
safeDoesFileExist path = do
    result <- liftIO (try (doesFileExist path) :: IO (Either IOException Bool))
    pure (either (const False) id result)

removeFileIfExists :: Path.OsPath -> IO ()
removeFileIfExists path = do
    exists <- doesFileExist path
    when exists (removeFile path)

tshow :: Show a => a -> Text
tshow = T.pack . show

pathToText :: Path.OsPath -> AppM Text
pathToText path = T.pack <$> liftIO (Path.decodeUtf path)

toWordCount :: Int -> Word
toWordCount = fromIntegral

tempFileSize :: Path.OsPath -> AppM (Maybe FileSize)
tempFileSize filePath = do
    sizeResult <- liftIO (try (getFileSize filePath) :: IO (Either IOException Integer))
    pure $
        case sizeResult of
            Left _ -> Nothing
            Right sizeInteger -> fileSizeFromIntegerMaybe sizeInteger

osPathToString :: Path.OsPath -> AppM String
osPathToString path = liftIO (Path.decodeUtf path)

displayPathFromRoot :: Path.OsPath -> Path.OsPath -> AppM Text
displayPathFromRoot rootPath fullPath = do
    let relativePath = Path.makeRelative rootPath fullPath
    relativeText <- pathToText relativePath
    pure $
        if T.null relativeText
            then "."
            else relativeText

renderLocalTimestamp :: TimeZone -> UTCTime -> IO Text
renderLocalTimestamp timeZone utcTime =
    pure . T.pack $ formatTime defaultTimeLocale "%F %T" (utcToLocalTime timeZone utcTime)

renderDetailedElapsed :: Integer -> Text
renderDetailedElapsed totalSeconds =
    T.pack $
        pad2 days
            <> "d "
            <> pad2 hours
            <> "h "
            <> pad2 minutes
            <> "m "
            <> pad2 seconds
            <> "s"
  where
    days = totalSeconds `div` 86400
    hours = (totalSeconds `mod` 86400) `div` 3600
    minutes = (totalSeconds `mod` 3600) `div` 60
    seconds = totalSeconds `mod` 60
    pad2 value
        | value < 10 = '0' : show value
        | otherwise = show value

renderPercentage3 :: Double -> String
renderPercentage3 value = showFFloat (Just 3) value "" <> "%"

renderSignedRateSI :: FileSize -> Integer -> String
renderSignedRateSI bytes elapsedSeconds
    | elapsedSeconds <= 0 = "0.000 B/s"
    | otherwise =
        let rate = fromIntegral bytes / fromIntegral elapsedSeconds :: Double
        in  renderScaledRateSI rate

renderScaledRateSI :: Double -> String
renderScaledRateSI value = renderScaledQuantitySI value ["B/s", "KB/s", "MB/s", "GB/s", "TB/s", "PB/s", "EB/s", "ZB/s", "YB/s"]

renderScaledQuantitySI :: Double -> [String] -> String
renderScaledQuantitySI value units = formatTo3 scaledValue <> " " <> unitLabel
  where
    (scaledValue, unitLabel) = scale value units

    scale :: Double -> [String] -> (Double, String)
    scale quantity [] = (quantity, "B")
    scale quantity [u] = (quantity, u)
    scale quantity (u:us)
        | quantity < 1000 = (quantity, u)
        | otherwise = scale (quantity / 1000) us

    formatTo3 :: Double -> String
    formatTo3 quantity = showFFloat (Just 3) quantity ""

runProcessCapture :: String -> [String] -> IO CommandResult
runProcessCapture executableText argsText = do
    (_, maybeStdout, maybeStderr, processHandle) <-
        createProcess
            (proc executableText argsText)
                { std_out = CreatePipe
                , std_err = CreatePipe
                }
    case (maybeStdout, maybeStderr) of
        (Just stdoutHandle, Just stderrHandle) ->
            waitForCommandResult stdoutHandle stderrHandle processHandle
                `finally` cleanupProcess stdoutHandle stderrHandle processHandle
        _ -> throwIO (userError "Failed to create ffmpeg output pipes.")

waitForCommandResult :: Handle -> Handle -> ProcessHandle -> IO CommandResult
waitForCommandResult stdoutHandle stderrHandle processHandle = do
    stdoutWorker <- async (T.pack <$> readHandleContents stdoutHandle)
    stderrWorker <- async (T.pack <$> readHandleContents stderrHandle)
    exitCode <- waitForProcess processHandle
    stdoutText <- wait stdoutWorker
    stderrText <- wait stderrWorker
    pure
        CommandResult
            { commandExitCode = exitCode
            , commandStdout = stdoutText
            , commandStderr = stderrText
            }

readHandleContents :: Handle -> IO String
readHandleContents handle =
    bracket
        (pure handle)
        hClose
        readContents
  where
        readContents stream = do
            contents <- hGetContents stream
            _ <- evaluate (length contents)
            pure contents

cleanupProcess :: Handle -> Handle -> ProcessHandle -> IO ()
cleanupProcess stdoutHandle stderrHandle processHandle = do
    _ <- try (terminateProcess processHandle) :: IO (Either IOException ())
    _ <- try (waitForProcess processHandle) :: IO (Either IOException ExitCode)
    _ <- try (hClose stdoutHandle) :: IO (Either IOException ())
    _ <- try (hClose stderrHandle) :: IO (Either IOException ())
    pure ()
