module Main (main) where

import Control.Exception (AsyncException (UserInterrupt), SomeException, fromException, mask, throwIO, throwTo, try)
import Control.Concurrent (myThreadId)
import Data.IORef (newIORef, readIORef, writeIORef)

import ShrinkVideos.App (buildRuntimeEnv, cancelCurrentFfmpeg, cleanupAndReport, runShrinkVideos)
import ShrinkVideos.Cli (parseOptions)
import System.Signal (installHandler, sigINT, sigTERM)

main :: IO ()
main = do
    options <- parseOptions
    env <- buildRuntimeEnv options
    mainThread <- myThreadId
    cleanupStarted <- newIORef False
    let onSignal _ = do
            cancelCurrentFfmpeg env
            isCleaningUp <- readIORef cleanupStarted
            if isCleaningUp
                then pure ()
                else throwTo mainThread UserInterrupt
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
