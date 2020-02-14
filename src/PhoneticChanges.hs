{-# LANGUAGE OverloadedStrings #-}

module PhoneticChanges where

import qualified Data.Text                     as T

type PhoneticWord = T.Text
type Translation = T.Text
type WrittenWord = T.Text
type WritingSystem = T.Text
data SoundLaw = SoundLaw T.Regex T.Text

-- PhoneticWord → Translation
translate :: PhoneticWord -> Translation

-- Writing system →   Phonetic Word → Written Word
getWritten :: WritingSystem -> PhoneticWord -> WrittenWord


-- Sound law -> Phonetic Word → Phonetic Word
changeWord :: SoundLaw -> PhoneticWord -> PhoneticWord

-- Sound laws -> Phonetic Word → Phonetic Word
migrateWord :: [SoundLaw] -> PhoneticWord -> PhoneticWord

-- Sound laws -> Phonetic Words → Phonetic Words
migrateWords :: [SoundLaw] -> [PhoneticWord] -> [PhoneticWord]


-- ////////
-- Prelude Text.Regex> :t subRegex 
-- subRegex :: Regex -> String -> String -> String
-- /////////
-- Prelude Text.Regex> subRegex (mkRegex "foo") "foobar" "123" "123bar"