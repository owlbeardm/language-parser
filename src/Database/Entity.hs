{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Database.Entity  where
--   (
--   WordText,
--   Comment,
--   SoundRegex,
--   Sound,
--   Priority,
--   Language (..),
--   Word (..),
--   Translation (..),
--   WordOrigin (..),
--   WordOriginFrom (..),
--   EvolveLaw (..)
--   )

import           ClassyPrelude       (Bool, Eq, Generic, Maybe, MonadIO, Show,
                                      Text, mconcat, return, show, unpack, ($))
import           Data.Aeson          (FromJSON, Object, ToJSON, object,
                                      parseJSON, toJSON, withObject, (.:), (.=))
import           Data.Aeson.Types    (Parser)
import           Data.Int
import           Data.Swagger        (ToSchema (..))
import           Database.Base
import           Database.Esqueleto
import           Database.Persist.TH

type WordText   = Text
type Comment    = Text
type SoundRegex = Text
type Sound      = Text
type Priority   = Int64

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Language sql=language_tbl
    lname LanguageName sql=name
    LanguageNameUnq lname
    deriving Generic
Word sql=word_tbl
    word WordText
    langId LanguageId
    partOfSpeech PartOfSpeech
    forgotten Bool
    WordWordPosLangIdUnq word partOfSpeech langId
    deriving Eq
    deriving Generic
Translation sql=translation_tbl
    fromWordId WordId
    toLangId LanguageId
    toWordId WordId Maybe
    comment Comment Maybe
    altTranslation Text Maybe
    deriving Generic
WordOrigin sql=word_origin_tbl
    wordId WordId
    comment Comment Maybe
    evolvedYn Bool
    migratedYn Bool
    combinedYn Bool
    derivatedYn Bool
    EvolveLawWordIdUnq wordId
    deriving Show
WordOriginFrom sql=word_origin_from_tbl
    wordFromId WordId
    originId WordOriginId
    deriving Show
EvolveLaw sql=evolve_law_tbl
    langFromId LanguageId
    langToId LanguageId
    soundRegexFrom SoundRegex
    soundTo Sound
    priority Priority
    deriving Show
|]


instance ToSchema Language

instance Show Language where
    show l = mconcat [ show $ languageLname l]

instance ToJSON Language where
  toJSON language = object
    [ "name" .= show (languageLname language)
    ]

instance ToJSON Translation where
  toJSON translation = object
    [ "comment" .= toJSON (translationComment translation),
      "altTranslation" .= toJSON (translationAltTranslation translation)
    ]

instance ToJSON Word where
  toJSON word = object
    [ "word" .= unpack (wordWord word),
      "partOfSpeech" .= show (wordPartOfSpeech word),
      "forgotten" .= wordForgotten word
    ]

-- instance ToSchema Word
-- instance ToSchema (Key Language)

instance FromJSON Language where
  parseJSON = withObject "Language" parseLanguage

parseLanguage :: Object -> Parser Language
parseLanguage o = do
  lName <- o .: "name"
  return Language
    { languageLname = lName
    }

findLangByName :: (MonadIO m) => LanguageName -> AppT m  (Maybe (Entity Language))
findLangByName name =  getBy $ LanguageNameUnq name
