{-# LANGUAGE TypeApplications #-}

module ParseSpec where

import ClassyPrelude
import Data.Char     (isSpace)
import Text.Read     (reads)

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck           ((==>))
import Test.QuickCheck.Modifiers (NonEmptyList(..), Positive(..))

import Musicd.Parse
import Musicd.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "parse" $ do
    it "handles file paths" $ parse "a/b/c" `shouldBe` File "a/b/c"
    it "handles empty lines" $ parse "" `shouldBe` Pause
    prop "starting with ./" $ \x ->
      let path = "./" <> x
       in parse (pack path) `shouldBe` File path
    prop "starting with ?" $ \(NonEmpty path) ->
      headMay path /= Just '?' &&
      (null . reads @Int $ path) ==> parse ("?" <> pack path) `shouldBe`
      Random 1 path
    prop "starting with ??" $ \(NonEmpty path) ->
      parse ("??" <> pack path) `shouldBe` Stream path
    prop "starting with ?s with a number" $ \(Positive num) (NonEmpty path) ->
      parse ("?" <> pack (show num) <> "?" <> pack path) `shouldBe`
      Random num path
    prop "starting with @" $ \term ->
      parse ("@" <> pack term) `shouldBe` YouTube term
    prop "starting with =" $ \(NonEmpty pat) ->
      doesn'tStartWithSpace pat ==> parse ("=" <> pack pat) `shouldBe` Glob pat
    prop "starting with = with spaces" $ \(Positive num) (NonEmpty pat) ->
      doesn'tStartWithSpace pat ==> parse ("=" <> replicate num ' ' <> pack pat) `shouldBe`
      Glob pat
    prop "starting with -" $ \(NonEmpty pat) ->
      doesn'tStartWithSpace pat ==> parse ("-" <> pack pat) `shouldBe`
      UseList pat
    prop "starting with - with spaces" $ \(Positive num) (NonEmpty pat) ->
      doesn'tStartWithSpace pat ==> parse ("-" <> replicate num ' ' <> pack pat) `shouldBe`
      UseList pat

doesn'tStartWithSpace :: String -> Bool
doesn'tStartWithSpace str =
  case headMay str of
    Just x
      | isSpace x -> False
    _ -> True
