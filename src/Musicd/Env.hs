module Musicd.Env (
    Env(..)
  , runWithEnv
  , musicAt
) where

import ClassyPrelude
import Musicd.Types  (MusicFile(..), Status)

-- | `Env` holds @musicd@'s global state at runtime.
data Env = Env {
    playlistFile :: FilePath
  , root         :: FilePath
  , cacheDir     :: FilePath
  , status       :: IORef Status
}

runWithEnv :: Env -> ReaderT Env m a -> m a
runWithEnv = flip runReaderT

musicAt :: MonadReader Env m => FilePath -> m MusicFile
musicAt path = getAtRoot <$> ask
  where
    getAtRoot Env {root} = MusicFile root path
