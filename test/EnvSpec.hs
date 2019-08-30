module EnvSpec where

import ClassyPrelude

import Test.Hspec
import Test.Hspec.QuickCheck

import Musicd.Env
import Musicd.Types (MusicFile(..), Status(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "musicAt" $
    prop "generate a MusicFile" $ \root playlistFile cacheDir path -> do
      status <- newIORef Stopped
      musicAt path Env {..} `shouldBe` MusicFile root path
