{-# LANGUAGE StrictData #-}
module Musicd.Types (
    PlaySpec(..)
  , Options(..)
  , Status(..)
  , MusicFile(..)
) where

import ClassyPrelude

data PlaySpec = File FilePath -- ^ A local file
              | Glob String -- ^ A glob pattern
              | YouTube String -- ^ A youtube search term
              | Random Int FilePath -- ^ Random files from a directory
              | Command Text [Text] -- ^ A specified command for musicd, with possible arguments (not currently in use)
              | Stream FilePath -- ^ Stream infinitely from a directory
              | UseList FilePath -- ^ Import a playlist
              | Pause -- ^ Pause until playlist is changed
              | None -- ^ Do nothing
  deriving (Eq, Show)

-- | `Status` represents whether `musicd` is
-- playing a file (and if so which file), or
-- is either paused or stopped.
data Status = Playing Text | Paused | Stopped
  deriving (Eq, Show)

-- | A file path relative to the music root.
data MusicFile = MusicFile {
    root       :: FilePath
  , path       :: FilePath
} deriving (Eq, Show)

-- | The command line options passed to `musicd`.
data Options = Options {
    foreground :: Bool
  , root       :: FilePath
  , playlist   :: FilePath
  , logFile    :: Maybe FilePath
  , ekgEnabled :: Bool
} deriving Show

