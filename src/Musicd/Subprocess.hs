module Musicd.Subprocess (
    proc
  , readProcess
  , runProcessIn
) where

import ClassyPrelude
import System.Process.Typed (ProcessConfig, proc, readProcessStdout_,
                             runProcess_, setWorkingDir)


readProcess :: MonadIO m => ProcessConfig stdin stdoutIgnored stderrIgnored -> m Text
readProcess = fmap (decodeUtf8 . toStrict) . readProcessStdout_

runProcessIn :: MonadIO m => FilePath -> ProcessConfig stdin stdout stderr -> m ()
runProcessIn dir = runProcess_ . setWorkingDir dir
