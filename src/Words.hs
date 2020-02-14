{-# LANGUAGE OverloadedStrings #-}

module Words where

import qualified Data.Text as T

type Lang = T.Text
type TranslationComments = [T.Text]
type LangWord = T.Text
type LangWords = [LangWord]
type SoundLaw = T.Text
type SoundLaws = [SoundLaw]


evolveLang :: Lang -> Lang -> Lang
evolveLang lang1 lang2 = 





