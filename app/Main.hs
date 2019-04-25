module Main where

import ClassyPrelude

import qualified Musicd
import           Musicd.Types (Options(..))

import           Options.Applicative
import           System.Directory       (getHomeDirectory)
import qualified System.Posix.Daemonize as D

main :: IO ()
main = do
  home              <- getHomeDirectory
  opts@Options {..} <- execParser $ info
    (optparser home <**> helper)
    (fullDesc <> header "Musicd - the music daemon")
  (if foreground then id else D.daemonize) (Musicd.run opts)

optparser :: FilePath -> Parser Options
optparser home =
  Options
    <$> switch
          (long "foreground" <> short 'f' <> help
            "Run in the foreground instead of as a daemon"
          )
    <*> strOption
          (  long "root"
          <> short 'r'
          <> metavar "PATH"
          <> action "directory"
          <> value (home </> "Music")
          <> help "Path to root music directory"
          )
    <*> strOption
          (  long "playlist"
          <> short 'p'
          <> metavar "PATH"
          <> action "file"
          <> value "playlist"
          <> help "Playlist path; relative to root directory"
          )
    <*> (optional . strOption)
          (long "log" <> short 'l' <> metavar "PATH" <> action "file" <> help
            "Path to log file"
          )
    <*> switch (long "ekg" <> help "Enable ekg monitoring on port 8888")
