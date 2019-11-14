module Musicd.Subprocess (
    proc
  , readProcess
  , runProcessIn
) where

import ClassyPrelude
import System.Exit          (ExitCode)
import System.Process.Typed (ProcessConfig, proc, readProcessStdout_,
                             runProcess, setWorkingDir)


readProcess :: MonadIO m => ProcessConfig stdin stdoutIgnored stderrIgnored -> m Text
readProcess = fmap (decodeUtf8 . toStrict) . readProcessStdout_

runProcessIn :: MonadIO m => FilePath -> ProcessConfig stdin stdout stderr -> m ExitCode
runProcessIn dir = runProcess . setWorkingDir dir
