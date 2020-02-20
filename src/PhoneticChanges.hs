{-# LANGUAGE OverloadedStrings #-}

module PhoneticChanges
  (
  ) where

import qualified Data.Text  as T
import qualified Text.Regex as R

type PhoneticWord = T.Text
type Translation = T.Text
type WrittenWord = T.Text

data SoundLaw =
  SoundLaw R.Regex T.Text

data WritingLaw =
  WritingLaw R.Regex T.Text

type WritingSystem = [WritingLaw]

-- -- PhoneticWord → Translation
-- translate :: PhoneticWord -> Translation
-- -- Writing system →   Phonetic Word → Written Word
-- getWritten :: WritingSystem -> PhoneticWord -> WrittenWord
changeWord :: PhoneticWord -> SoundLaw -> PhoneticWord
changeWord word (SoundLaw regex changedPart) =
  T.pack (R.subRegex regex (T.unpack word) (T.unpack changedPart))

migrateWord :: PhoneticWord -> [SoundLaw] -> PhoneticWord
migrateWord word soundLaws = foldl changeWord word soundLaws

migrateWords :: [PhoneticWord] -> [SoundLaw] -> [PhoneticWord]
migrateWords words laws = map ((flip migrateWord) laws) words

who = T.pack "kʷei"

nine = T.pack "H1néwn̥"

firstRegex = R.mkRegex "ei"

secondRegex = R.mkRegex "^H1"

thirdRegex = R.mkRegex "H1"

firstSl = SoundLaw firstRegex "ē"

secondSl = SoundLaw secondRegex "a"

thirdSl = SoundLaw thirdRegex ""

listSl = [firstSl, secondSl, thirdSl]

listW = [who, nine]
-- ei      > ē
-- H1#     > a
-- H1      >
