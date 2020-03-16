{-# LANGUAGE TemplateHaskell #-}

module Database.Word where

import           ClassyPrelude
import           Database.Persist.TH


data PartOfSpeech = Adjective
    | Adverb
    | Conjunction
    | Noun
    | Numeral
    | Preposition
    | Pronoun
    | Verb
    deriving (Eq, Enum, Read, Show)

-- instance Show PartOfSpeech where
--   show Adjective   = "adj."
--   show Adverb      = "adv."
--   show Conjunction = "cnj."
--   show Noun        = "n."
--   show Numeral     = "num."
--   show Preposition = "prep."
--   show Pronoun     = "pro."
--   show Verb        = "v."

derivePersistField "PartOfSpeech"
