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
migrateWord = foldl changeWord

migrateWords :: [PhoneticWord] -> [SoundLaw] -> [PhoneticWord]
migrateWords words laws = map (`migrateWord` laws) words

who = T.pack "kʷei"

nine = T.pack "H1néwn̥"

firstSl = SoundLaw (R.mkRegex "ei") "ē"

secondSl = SoundLaw (R.mkRegex "^H1") "a"

thirdSl = SoundLaw (R.mkRegex "H1") ""

listSl = [firstSl, secondSl, thirdSl]

listW = [who, nine]
-- ei      > ē
-- H1#     > a
-- H1      >
