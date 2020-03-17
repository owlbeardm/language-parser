module Database.Word where

import           ClassyPrelude hiding (Word)
-- import           Control.Monad.Logger         (LoggingT, NoLoggingT, logErrorN,
--                                                logErrorNS, runNoLoggingT,
--                                                runStderrLoggingT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader 
import           Database.Base
import           Database.Entity
import           Database.Esqueleto
import           Database.Persist.TH

addWord :: (MonadIO m, MonadLogger m) => Text -> PartOfSpeech -> LanguageName -> AppT m (Maybe (Key Word))
addWord word pos langName = do
  lang <- getBy $ LanguageNameUnq langName
  case lang of 
    Nothing -> do
      logErrorNS "addWord" "There is no such lang in the database"
      return Nothing 
    (Just l) -> insertUnique $ Word word (entityKey l) pos

listWordsByLang :: (MonadIO m) => LanguageName -> AppT m [Entity Word]
listWordsByLang langName = select $ from $ \(word,lang) -> do
      where_ (word ^. WordLangId ==. lang ^. LanguageId &&. 
              lang ^. LanguageLname ==. val langName )
      return word