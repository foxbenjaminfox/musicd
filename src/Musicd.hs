module Musicd (
  run
) where

import           ClassyPrelude
import           Control.Monad.Logger
import           Musicd.Env
import           Musicd.Parse
import           Musicd.Types
import           System.Directory         (XdgDirectory(..), getXdgDirectory)
import           System.FilePath          (makeRelative)
import qualified System.FilePath.Glob     as G
import qualified System.INotify           as Notify
import           System.Process.Typed
import           System.Random.Shuffle    (shuffleM)
import qualified System.Remote.Monitoring as EKG

-- | Runs @musicd@'s main loop. See the <https://github.com/foxbenjaminfox/musicd/blob/master/README.md README> for details about how @musicd@ works and what options it accepts.
run :: Options -> IO ()
run opts = do
  let Options {..} = opts
      playlistFile = root </> playlist

  cacheDir <- getXdgDirectory XdgCache "musicd"
  status <- newIORef Stopped

  when ekgEnabled . void $ EKG.forkServer "localhost" 8888

  let runLogger = maybe runStderrLoggingT runFileLoggingT logFile
  runLogger . forever $ getPlaylist Env {..}

getPlaylist ::  (MonadIO m, MonadLogger m) =>Env -> m ()
getPlaylist Env {..} =
  lines <$> readFileUtf8 playlistFile >>= \case
    [] -> do
      writeIORef status Stopped
      logInfoN "Waiting for playlist..."
      waitForFileChange playlistFile
    playlist@(x:xs) ->
      readIORef status >>= \case
        Paused | parse x == Pause -> waitForFileChange playlistFile
        y | y == Playing x -> do
            writeFileUtf8 playlistFile $ unlines xs
            writeIORef status Stopped
          | otherwise -> do
              expandPlaylist playlist Env {..}
              writeIORef status $ Playing x
              playItem x Env {..}

expandPlaylist :: (MonadIO m, MonadLogger m) => [Text] -> Env -> m ()
expandPlaylist playlist Env {..} = do
  revised <- forM (zip [0::Int ..] playlist) $ \(idx, line) ->
    case parse line of
      Random num path -> do
        out <- liftIO . shuffleM =<< lines . decodeUtf8 . toStrict <$> readProcessStdout_ (proc "find" ["-L", root </> path, "-type", "f"])
        pure . take num . map (pack . makeRelative root . unpack) $ out
      Glob pat ->
        sort . fmap (pack . makeRelative root) <$> liftIO (G.globDir1 (G.compile pat) root)
      Stream path | idx == 0 -> pure ["?" <> pack path, line]
      UseList path -> lines <$> readFileUtf8 (root </> path)
      _ -> pure [line]
  writeFileUtf8 playlistFile (unlines . concat $ revised)

playItem :: (MonadIO m, MonadLogger m) => Text -> Env -> m ()
playItem x Env {..} = case parse x of
    File path -> do
      logInfoN $ "Playing file: " <> pack path
      playFile $ MusicFile root path
    YouTube term ->
      runProcess_ (setWorkingDir cacheDir $ proc "youtube-dl" ["--extract-audio", "--audio-format", "mp3", "--exec", "play {}; rm {}", "ytsearch1:" <> term])
    Pause -> writeIORef status Paused
    _ -> pure ()

playFile :: MonadIO m => MusicFile -> m ()
playFile (MusicFile root file) = void $ runProcess (setWorkingDir root $ proc "play" [file])

waitForFileChange :: MonadIO m => FilePath -> m ()
waitForFileChange filename = liftIO . Notify.withINotify $ \notifier -> do
  semaphore <- newEmptyMVar
  void $
    Notify.addWatch notifier
      [Notify.AllEvents]
      (encodeUtf8 . pack $ filename)
      (const $ putMVar semaphore ())
  takeMVar semaphore
