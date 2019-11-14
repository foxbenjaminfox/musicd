module Musicd.Util (
    createDir
  , globIn
  , iforM
  , shuffle
) where

import           ClassyPrelude
import           System.Directory      (createDirectoryIfMissing)
import qualified System.FilePath.Glob  as G
import           System.Random.Shuffle (shuffleM)


createDir :: MonadIO m => FilePath -> m ()
createDir = liftIO . createDirectoryIfMissing True

globIn :: MonadIO m => String -> FilePath -> m [FilePath]
globIn pat root = liftIO (G.globDir1 (G.compile pat) root)


shuffle :: MonadIO m => [Text] -> m [Text]
shuffle = liftIO . shuffleM

iforM :: Monad m => [a] -> ((Int, a) -> m b) -> m [b]
iforM = forM . zip [0::Int ..]
