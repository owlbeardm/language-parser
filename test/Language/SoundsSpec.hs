module Language.SoundsSpec where

import           ClassyPrelude
import           Language.Sounds
import           Test.Tasty
import           Test.Tasty.Hspec

soundsSpecs :: Spec
soundsSpecs = describe "Language.Sounds" $ do
  spec_isVowel
  spec_splitOnAnyOf

spec_isVowel :: Spec
spec_isVowel =
  describe "isVowel" $ do
    it "'ñ' isn't a vowel" $
      isVowel 'ñ' `shouldBe` False
    it "'e' is a vowel" $
      isVowel 'e' `shouldBe` True

spec_splitOnAnyOf :: Spec
spec_splitOnAnyOf =
  describe "splitOnAnyOf" $ do
    it "\"deluela\" does not split on \"obc\"" $
      splitOnAnyOf ["obc"] "deluela" `shouldBe` ["deluela"]
    it "\"deluela\" splits on \"a\", \"d\", \"l\"" $
      splitOnAnyOf ["a", "d", "l"] "deluela" `shouldBe` ["","e","ue","",""]
    it "\"deluela\" does not split on empty array" $
      splitOnAnyOf [] "deluela" `shouldBe` ["deluela"]