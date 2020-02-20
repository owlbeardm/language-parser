{-# LANGUAGE OverloadedStrings #-}

module Words where

import qualified Data.Text as T

<<<<<<< HEAD
-- type TranslationComments = [T.Text]
-- type Word = Data.Text
-- type Words = [Word]
-- -- SoundLaw = ?
-- type SoundLaws = [SoundLaw]

-- migrateWords :: Words -> SoundLaws -> Words
-- migrateWord :: Word -> SoundLaws -> Word
=======
type Lang = T.Text
type TranslationComments = [T.Text]
type LangWord = T.Text
type LangWords = [LangWord]
type SoundLaw = T.Text
type SoundLaws = [SoundLaw]


evolveLang :: Lang -> Lang -> Lang
evolveLang lang1 lang2 = 
>>>>>>> master





