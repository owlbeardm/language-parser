{-# LANGUAGE TemplateHaskell #-}

module Database.Language where

import           ClassyPrelude
import           Database.Persist.TH


data LanguageName = ProtoMaterial
    | ProtoMonster
    | English
    deriving (Eq, Enum, Read)

instance Show LanguageName where
  show ProtoMaterial = "Proto-Material"
  show ProtoMonster = "Proto-Monster"
  show English = "English"

derivePersistField "LanguageName"