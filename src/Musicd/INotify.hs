module Musicd.INotify (
  waitForFileChange
) where

import           ClassyPrelude
import qualified System.INotify as Notify

waitForFileChange :: MonadIO m => FilePath -> m ()
waitForFileChange filename = liftIO . Notify.withINotify $ \notifier -> do
  semaphore <- newEmptyMVar
  void $
    Notify.addWatch notifier
      [Notify.AllEvents]
      (encodeUtf8 . pack $ filename)
      (const $ putMVar semaphore ())
  takeMVar semaphore
