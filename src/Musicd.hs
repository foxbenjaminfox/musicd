module Musicd (
  run
) where

import           ClassyPrelude
import           Control.Monad.Logger
import           Musicd.Env
import           Musicd.INotify
import           Musicd.Parse
import           Musicd.Subprocess
import           Musicd.Types
import           Musicd.Util
import           System.Directory         (XdgDirectory(..), getXdgDirectory)
import           System.FilePath          (makeRelative)
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

  runLogger . runWithEnv Env {..} . forever $ getPlaylist

getPlaylist ::  (MonadReader Env m, MonadIO m, MonadLogger m) => m ()
getPlaylist = do
  Env { playlistFile, status } <- ask
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
              expandPlaylist playlist
              writeIORef status $ Playing x
              playItem x

expandPlaylist :: (MonadReader Env m, MonadIO m, MonadLogger m) => [Text] -> m ()
expandPlaylist playlist = do
  Env { playlistFile, root } <- ask
  revised <- iforM playlist $ \(idx, line) ->
    case parse line of
      Random num path -> do
        logInfoN $ "Selecting " <> pack (show num) <> " random files from \"" <> pack path <> "\"."
        expandRandom num path root
      Glob pat -> do
        logInfoN $ "Expanding glob \"" <> pack pat <> "\"."
        sort . fmap (pack . makeRelative root) <$> globIn pat root
      Stream path | idx == 0 ->
        pure ["?" <> pack path, line]
      UseList path ->
        lines <$> readFileUtf8 (root </> path)
      _ -> pure [line]
  writeFileUtf8 playlistFile (unlines . concat $ revised)

playItem :: (MonadReader Env m, MonadIO m, MonadLogger m) => Text -> m ()
playItem x = case parse x of
    File path -> do
      logInfoN $ "Playing file: " <> pack path
      playFile =<< musicAt path
    YouTube searchTerm -> do
      cacheDir <- asks cacheDir
      createDir cacheDir
      playYoutube cacheDir searchTerm
    Pause -> do
      status <- asks status
      writeIORef status Paused
    _ -> pure ()

playFile :: MonadIO m => MusicFile -> m ()
playFile (MusicFile root file) = void . runProcessIn root $ proc "play" [file]

playYoutube :: (MonadIO m, MonadLogger m) => FilePath -> String -> m ()
playYoutube dir search = do
  logInfoN $ "Searching \"" <> pack search <> "\" on YouTube."
  void . runProcessIn dir $
    proc "youtube-dl" ["--extract-audio", "--audio-format", "mp3", "--exec", "play {}; rm {}", "ytsearch1:" <> search]

findFilesIn :: MonadIO m => FilePath -> m [Text]
findFilesIn path = lines <$> readProcess (proc "find" ["-L", path, "-type", "f"])

expandRandom :: MonadIO m => Int -> FilePath -> FilePath -> m [Text]
expandRandom num path root = do
  files <- findFilesIn (root </> path) >>= shuffle
  let relativeFiles = map (pack . makeRelative root . unpack) files
  pure . take num $ relativeFiles
