{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception (AsyncException (UserInterrupt), SomeException, fromException, mask, throwIO, throwTo, try)
import Control.Concurrent (myThreadId)
import Control.Monad (filterM)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))

import ShrinkVideos.App (buildRuntimeEnv, cancelCurrentFfmpeg, cleanupAndReport, runShrinkVideos)
import ShrinkVideos.Cli (parseOptions)
import System.Directory.OsPath qualified as Directory
import System.Exit (exitFailure)
import System.OsPath qualified as Path
import System.Signal (installHandler, sigINT, sigTERM)

newtype ExternalDependency = ExternalDependency
    { renderExternalDependency :: Path.OsPath
    }


{- |
@since 1.0.0
/O(1)/, excluding command execution performed by the program after startup.
/O(1)/, excluding allocations performed by downstream processing.

Program entrypoint. Parses command-line arguments, validates required external
dependencies, installs signal handlers, and then runs the video shrink workflow.

Examples:

@
$ Shrink-Videos --directory ./videos
@
-}
main :: IO ()
main = do
    options <- parseOptions
    validateRequiredDependencies
    env <- buildRuntimeEnv options
    mainThread <- myThreadId
    cleanupStarted <- newIORef False
    let onSignal _ = do
            cancelCurrentFfmpeg env
            isCleaningUp <- readIORef cleanupStarted
            case isCleaningUp of
                False -> throwTo mainThread UserInterrupt
                True -> pure ()
    installHandler sigINT onSignal
    installHandler sigTERM onSignal

    runResult <- mask $ \restore -> do
        result <- try (restore (runShrinkVideos env)) :: IO (Either SomeException ())
        writeIORef cleanupStarted True
        cleanupAndReport env
        pure result

    case runResult of
        Right () -> pure ()
        Left ex ->
            case fromException ex of
                Just UserInterrupt -> pure ()
                Just _ -> throwIO ex
                Nothing -> throwIO ex


validateRequiredDependencies :: IO ()
validateRequiredDependencies = do
    missingDependencies <- findMissingDependencies requiredDependencies
    case missingDependencies of
        [] -> pure ()
        firstMissingDependency : remainingMissingDependencies -> do
            renderMissingDependencyMessage (firstMissingDependency :| remainingMissingDependencies) >>= putStrLn
            exitFailure


findMissingDependencies :: [ExternalDependency] -> IO [ExternalDependency]
findMissingDependencies =
    filterM
        ( \dependency ->
            fmap
                ( \case
                    Nothing -> True
                    Just _ -> False
                )
                (Directory.findExecutable (renderExternalDependency dependency))
        )


requiredDependencies :: [ExternalDependency]
requiredDependencies =
    [ ExternalDependency (Path.unsafeEncodeUtf "ffmpeg")
    , ExternalDependency (Path.unsafeEncodeUtf "ffprobe")
    ]


renderMissingDependencyMessage :: NonEmpty ExternalDependency -> IO String
renderMissingDependencyMessage missingDependencies = do
    missingDependencyNames <- traverse (Path.decodeUtf . renderExternalDependency) missingDependencies
    pure
        ( "Missing required external dependenc"
            <> dependencySuffix missingDependencyNames
            <> ": "
            <> joinDependencyNames missingDependencyNames
            <> ". Please install "
            <> installationTarget missingDependencyNames
            <> " and ensure "
            <> pathTarget missingDependencyNames
            <> " available on $PATH."
        )


dependencySuffix :: NonEmpty String -> String
dependencySuffix = \case
    _ :| [] -> "y"
    _ -> "ies"


installationTarget :: NonEmpty String -> String
installationTarget dependencyNames = case dependencyNames of
    _ :| [] -> joinDependencyNames dependencyNames
    _ -> "them"


pathTarget :: NonEmpty String -> String
pathTarget = \case
    _ :| [] -> "it is"
    _ -> "they are"


joinDependencyNames :: NonEmpty String -> String
joinDependencyNames = \case
    dependencyName :| [] -> dependencyName
    firstDependencyName :| [secondDependencyName] ->
        firstDependencyName <> " and " <> secondDependencyName
    firstDependencyName :| remainingDependencyNames ->
        firstDependencyName <> ", " <> joinDependencyNames (nonEmptyFromList remainingDependencyNames)


nonEmptyFromList :: [item] -> NonEmpty item
nonEmptyFromList = \case
    firstItem : remainingItems -> firstItem :| remainingItems
    [] ->
        error
            "nonEmptyFromList received an empty list despite being called only in non-empty contexts."
