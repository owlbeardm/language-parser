{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module HTTP.Utility where

import           ClassyPrelude        (Bool (..), Generic, Int64, Maybe (..),
                                       Text, show, unpack)
import           Data.Aeson           (FromJSON, ToJSON, object, toJSON, (.=))
import           Database.Base        (LanguageName (..), PartOfSpeech (..))
import           Database.Entity      (Language, Word, WordText, wordForgotten,
                                       wordPartOfSpeech, wordWord)
import           Database.Translation (WordSource, WordTranslation)

type WordDescriptionAPI = (Word, [Language], [WordTranslation], [WordSource])

data AddWordJSON = AddWordJSON { lang      :: LanguageName
                               , pos       :: PartOfSpeech
                               , wordText  :: WordText
                               , forgotten :: Bool
                               }
                               deriving (Generic, ToJSON, FromJSON)

data AddWordTranslationJSON = AddWordTranslationJSON { translation      :: Maybe AddWordJSON
                                                     , wordFromId       :: Int64
                                                     , langTo           :: LanguageName
                                                     , altTranslation   :: Maybe Text
                                                     , isAltTranslation :: Bool
                                                     , comment          :: Maybe Text
                                                     }
                                                     deriving (Generic, ToJSON, FromJSON)

data TraceWordReq = TraceWordReq { wordTrace  :: WordText
                                 , langs :: [LanguageName]
                                 }
                                 deriving (Generic, ToJSON, FromJSON)

data WordJSON = WordJSON Int64 Word

instance ToJSON WordJSON where
  toJSON (WordJSON key word) = object
    [ "id" .= key,
      "word" .= unpack (wordWord word),
      "partOfSpeech" .= show (wordPartOfSpeech word),
      "forgotten" .= wordForgotten word
    ]


checkadded :: Maybe a -> Bool
checkadded Nothing = False
checkadded _       = True
