{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module HTTP.Utility where

import           ClassyPrelude        (Bool (..), Generic, Int64, Maybe (..),
                                       Text, map, (.))
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Swagger         (ToSchema (..))
import           Database.Base        (LanguageName (..), PartOfSpeech (..))
import           Database.Entity      (Comment, Translation, Word,
                                       WordText, languageLname, wordForgotten,
                                       wordPartOfSpeech, wordWord)
import qualified Database.Entity      as DE (translationAltTranslation,
                                             translationComment,
                                             translationFromWordId,
                                             translationToLangId,
                                             translationToWordId)
import           Database.Esqueleto   (entityKey, entityVal, fromSqlKey)
import           Database.Translation (WordAndLang, WordDescription, WordSource,
                                       WordTranslation)

type WordTranslationAPI = (TranslationAPI, LanguageName, Maybe WordJSON)
type WordAndLangAPI = (WordJSON, LanguageName)
type WordSourceAPI = (WordAndLangAPI, [WordTranslationAPI])
type WordDescriptionAPI = (WordJSON, [LanguageName], [WordTranslationAPI], [WordSourceAPI])

data TranslationAPI = TranslationAPI { fromWordId                :: Int64
                                     , toLangId                  :: Int64
                                     , toWordId                  :: Maybe Int64
                                     , translationComment        :: Maybe Comment
                                     , translationAltTranslation :: Maybe Text
                                     }
                                     deriving (Generic, ToJSON, FromJSON, ToSchema)

data AddWordJSON = AddWordJSON { lang          :: LanguageName
                               , pos           :: PartOfSpeech
                               , wordText      :: WordText
                               , makeForgotten :: Bool
                               }
                               deriving (Generic, ToJSON, FromJSON, ToSchema)

data AddWordTranslationJSON = AddWordTranslationJSON { translation      :: Maybe AddWordJSON
                                                     , wordFromId       :: Int64
                                                     , langTo           :: LanguageName
                                                     , altTranslation   :: Maybe Text
                                                     , isAltTranslation :: Bool
                                                     , comment          :: Maybe Text
                                                     }
                                                     deriving (Generic, ToJSON, FromJSON, ToSchema)

data TraceWordReq = TraceWordReq { wordTrace :: WordText
                                 , langs     :: [LanguageName]
                                 }
                                 deriving (Generic, ToJSON, FromJSON, ToSchema)

data WordJSON = WordJSON { id           :: Maybe Int64
                         , word         :: WordText
                         , partOfSpeech :: PartOfSpeech
                         , forgotten    :: Bool
                         } deriving (Generic, ToJSON, FromJSON, ToSchema)

convertWordToWordJson :: (Int64, Word) -> WordJSON
convertWordToWordJson (i, wrd) = WordJSON (Just i) (wordWord wrd) (wordPartOfSpeech wrd) (wordForgotten wrd)

convertToWordJson :: Word -> WordJSON
convertToWordJson wrd = WordJSON Nothing (wordWord wrd) (wordPartOfSpeech wrd) (wordForgotten wrd)

convertTranslationAPI :: Translation -> TranslationAPI
convertTranslationAPI tr = TranslationAPI ((fromSqlKey . DE.translationFromWordId) tr) ((fromSqlKey . DE.translationToLangId) tr) (map fromSqlKey (DE.translationToWordId tr)) (DE.translationComment tr) (DE.translationAltTranslation tr)

-- type WordTranslation = (Translation, Language, Maybe Database.Entity.Word)
-- type WordAndLang = (Database.Entity.Word, Language)
-- type WordSource = (WordAndLang, [WordTranslation])
-- type WordDescription = (Entity Database.Entity.Word, [Language], [WordTranslation], [WordSource])

convertWordTranslationAPI :: WordTranslation -> WordTranslationAPI
convertWordTranslationAPI (tr, ln, mwrd) = (convertTranslationAPI tr, languageLname ln, map convertToWordJson mwrd)

convertWordAndLangAPI :: WordAndLang -> WordAndLangAPI
convertWordAndLangAPI (wrd, ln) = (convertToWordJson wrd, languageLname ln)

convertWordSourceAPI :: WordSource -> WordSourceAPI
convertWordSourceAPI (wl, listwt) = (convertWordAndLangAPI wl, map convertWordTranslationAPI listwt)

convertWordDescriptionAPI :: WordDescription -> WordDescriptionAPI
convertWordDescriptionAPI (eWord, lLn, lWt, lWs) = (
  (convertWordToWordJson . toTuple) eWord,
  map languageLname lLn,
  map convertWordTranslationAPI lWt,
  map convertWordSourceAPI lWs)
  where
    toTuple eW = (toInt eW, entityVal eW)
    toInt = fromSqlKey . entityKey

checkadded :: Maybe a -> Bool
checkadded Nothing = False
checkadded _       = True
