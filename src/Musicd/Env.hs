module Musicd.Env (
  Env(..)
) where

import           ClassyPrelude
import           Musicd.Types (Status)

-- | `Env` holds @musicd@'s global state at runtime.
data Env = Env {
    playlistFile         :: FilePath
  , root                 :: FilePath
  , cacheDir             :: FilePath
  , status               :: IORef Status
}
