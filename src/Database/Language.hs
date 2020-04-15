module Database.Language where

import           ClassyPrelude        hiding (Word)
import qualified Data.Text            as T
import qualified Text.Regex           as R
-- import           Control.Exception         (throw)
-- import           Control.Monad.Trans.Maybe
import           Control.Monad.Logger
import           Database.Base
import           Database.Word
import           Database.Entity
import           Database.Esqueleto

returnLang :: (MonadIO m) => Int64 -> AppT m [Entity Language]
returnLang i = select $ from $ \lang -> do
   where_ (lang ^. LanguageId ==. val (toSqlKey i))
   return lang

listLangs :: (MonadIO m) => AppT m [Entity Language]
listLangs = select $ from $ \lang -> return lang

addLang :: (MonadIO m) => LanguageName -> AppT m  (Key Language)
addLang name = insert $ Language name

getLangByName :: (MonadIO m) => LanguageName -> AppT m  (Maybe (Entity Language))
getLangByName name =  getBy $ LanguageNameUnq name

getWord :: (MonadIO m) => Entity Language -> Text -> PartOfSpeech -> AppT m  (Maybe (Entity Word))
getWord eLang word pos = getBy $ WordWordPosLangIdUnq word pos (entityKey eLang)

insertEvolvedWord :: (MonadIO m) => Text -> PartOfSpeech -> Key Word -> Key Language -> AppT m (Key Word)
insertEvolvedWord textToAdd pos wfKey langToKey = do
   wordToKey <- insert $ Word textToAdd langToKey pos
   wordOriginKey <- insert $ WordOrigin wordToKey Nothing True False False False
   _ <- insert $ WordOriginFrom wfKey wordOriginKey
   return wordToKey

listEvolveLawsByLangs :: (MonadIO m) => LanguageName -> LanguageName -> AppT m [Entity EvolveLaw]
listEvolveLawsByLangs langNameFrom langNameTo = select $ from $ \(evolveLaw,langFrom,langTo) -> do
      where_ (evolveLaw ^. EvolveLawLangFromId ==. langFrom ^. LanguageId &&.
              langFrom ^. LanguageLname ==. val langNameFrom &&.
              evolveLaw ^. EvolveLawLangToId ==. langTo ^. LanguageId &&.
              langTo ^. LanguageLname ==. val langNameTo)
              --order by prior
      return evolveLaw

evolveWordText :: Text -> [EvolveLaw] -> Text
evolveWordText = foldl' changeWord

changeWord :: Text -> EvolveLaw -> Text
changeWord wordText law = T.pack $ R.subRegex regex word soundTo
   where
      regex = (R.mkRegex . T.unpack . evolveLawSoundRegexFrom) law
      soundTo = (T.unpack . evolveLawSoundTo) law
      word = T.unpack wordText

evolvedWord :: (MonadIO m) => [EvolveLaw] -> Entity Word -> Key Language -> AppT m (Key Word)
evolvedWord laws eWordFrom = insertEvolvedWord evolvedText ((wordPartOfSpeech . entityVal) eWordFrom) (entityKey eWordFrom)
   where
      evolvedText = evolveWordText ((wordWord . entityVal) eWordFrom) laws

evolveLang :: (MonadIO m) => LanguageName -> LanguageName -> AppT m (Maybe [Key Word])
evolveLang langNameFrom langNameTo = do
   words <- listNotEvolvedWordsByLangFromAndTo langNameFrom langNameTo
   evolveLaws <- listEvolveLawsByLangs langNameFrom langNameTo
   keys <- mapM (\x -> evolvedWord (map entityVal evolveLaws) x (toSqlKey 1)) words
   return $ Just keys

-- getAll words by lang
-- for each word
--    if no evolved word than evolve
--       for each word change text
--       for each word new word