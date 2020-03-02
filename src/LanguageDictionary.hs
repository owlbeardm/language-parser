{-# LANGUAGE OverloadedStrings #-}

module LanguageDictionary where

import           ClassyPrelude
import qualified Data.Text as T

data TranslationLang
  = En
  | Ru
  deriving (Show)

type TranslationComments = [T.Text]

data TranslationData =
  TranslationData T.Text (Maybe TranslationComments)
  deriving (Show)

data Translation =
  Translation TranslationLang [TranslationData]
  deriving (Show)

type LangWordSettings = [T.Text]

data LangWord =
  LangWord T.Text (Maybe LangWordSettings)
  deriving (Show)

data DictionaryRecord =
  DictionaryRecord LangWord (Maybe [Translation])
  deriving (Show)

data Dictionary =
  Dictionary [DictionaryRecord]
  deriving (Show)

-- parseDictionary :: T.Text -> Dictionary
--
--
dic =
  Dictionary
    [ DictionaryRecord
        (LangWord "hell0" Nothing)
        (Just
           [ Translation
               En
               [TranslationData "hello" (Just ["standart", "no bd"])]
           , Translation Ru [TranslationData "привет" Nothing]
           ])
    , DictionaryRecord (LangWord "by3" (Just ["what a settings"])) Nothing
    , DictionaryRecord (LangWord "di3" Nothing) Nothing
    ]

dic' (Dictionary ((DictionaryRecord x y):xs)) = x

dicSh = dic' dic
