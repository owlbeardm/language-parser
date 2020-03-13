{-# LANGUAGE TemplateHaskell #-}

module Database.Language where

import           ClassyPrelude
import           Database.Persist.TH


data LanguageName = Aboleth
                  | Alko
                  | Dragon
                  | Dwarven
                  | Elven
                  | English
                  | Halfling
                  | Infernal
                  | Kobold
                  | Necril
                  | Old_Runic
                  | Orkish
                  | ProtoCreation
                  | ProtoDragon
                  | ProtoDwarven
                  | ProtoElven
                  | ProtoHuman
                  | ProtoMaterial
                  | ProtoMonster
                  | ProtoOrk
                  | ProtoTengu
                  | Sylvan
    deriving (Eq, Enum, Read)

instance Show LanguageName where
  show ProtoMaterial = "Proto-Material"
  show ProtoMonster = "Proto-Monster"
  show English = "English"
  show _ = "Undef Lang"

derivePersistField "LanguageName"