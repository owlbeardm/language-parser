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
                  | OldRunic
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
    deriving (Eq, Enum, Read, Show)

-- instance Show LanguageName where
--   show Aboleth       = "Aboleth"
--   show Alko          = "Alko"
--   show Dragon        = "Dragon"
--   show Dwarven       = "Dwarven"
--   show Elven         = "Elven"
--   show English       = "English"
--   show Halfling      = "Halfling"
--   show Infernal      = "Infernal"
--   show Kobold        = "Kobold"
--   show Necril        = "Necril"
--   show OldRunic      = "Old Runic"
--   show Orkish        = "Orkish"
--   show ProtoCreation = "Proto-Creation"
--   show ProtoDragon   = "Proto-Dragon"
--   show ProtoDwarven  = "Proto-Dwarven"
--   show ProtoElven    = "Proto-Elven"
--   show ProtoHuman    = "Proto-Human"
--   show ProtoMaterial = "Proto-Material"
--   show ProtoMonster  = "Proto-Monster"
--   show ProtoOrk      = "Proto-Ork"
--   show ProtoTengu    = "Proto-Tengu"
--   show Sylvan        = "Sylvan"
--   show _             = "Undef Lang"

derivePersistField "LanguageName"
