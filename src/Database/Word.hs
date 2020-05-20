module Database.Word where

import           ClassyPrelude        hiding (Word, delete, groupBy, isNothing,
                                       on, (\\))
import           Control.Monad.Logger
import           Data.List            ((\\))
import           Database.Base
import           Database.Entity
import           Database.Esqueleto

findWordById :: (MonadIO m) => Int64 -> AppT m [Entity Word]
findWordById i = select $ from $ \word -> do
   where_ (word ^. WordId ==. val (toSqlKey i))
   return word

addWord :: (MonadIO m) => WordText -> Key Language -> PartOfSpeech -> Bool ->  AppT m (Key Word)
addWord word langKey pos forgotten  = insert $ Word word langKey pos forgotten

addWordByLangName :: (MonadIO m, MonadLogger m) => LanguageName -> WordText -> PartOfSpeech ->  AppT m (Maybe (Key Word))
addWordByLangName langName word pos  = do
  lang <- findLangByName langName
  case lang of
    Nothing -> do
      logErrorNS "addWordByLangName" "There is no such lang in the database"
      return Nothing
    (Just l) -> do
      key <- addWord word (entityKey l) pos False
      return $ Just key

addEvolvedWord :: (MonadIO m) => WordText -> PartOfSpeech -> Key Word -> Key Language -> AppT m (Key Word)
addEvolvedWord textToAdd pos wfKey langToKey = do
   wordToKey <- addWord textToAdd langToKey pos False
   wordOriginKey <- insert $ WordOrigin wordToKey Nothing True False False False
   _ <- insert $ WordOriginFrom wfKey wordOriginKey
   return wordToKey

findWord :: (MonadIO m) => Entity Language -> WordText -> PartOfSpeech -> AppT m  (Maybe (Entity Word))
findWord eLang word pos = getBy $ WordWordPosLangIdUnq word pos (entityKey eLang)

findWordsByText :: (MonadIO m) => WordText-> AppT m  [Entity Word]
findWordsByText text =
    select $
    from $ \(word) -> do
      where_ (word ^. WordWord ==. val text)
      return word

listWordsByLang :: (MonadIO m) => LanguageName -> AppT m [Entity Word]
listWordsByLang langName = select $ from $ \(word,lang) -> do
      where_ (word ^. WordLangId ==. lang ^. LanguageId &&.
              lang ^. LanguageLname ==. val langName )
      return word

listNotForgottenWordsByLang :: (MonadIO m) => LanguageName -> AppT m [Entity Word]
listNotForgottenWordsByLang langName = select $ from $ \(word,lang) -> do
      where_ (word ^. WordLangId ==. lang ^. LanguageId &&.
              word ^. WordForgotten ==. val True &&.
              lang ^. LanguageLname ==. val langName )
      return word

listNotEvolvedWordsByLangFromAndTo :: (MonadIO m) => LanguageName -> LanguageName -> AppT m [Entity Word]
listNotEvolvedWordsByLangFromAndTo langNameFrom langNameTo = do
  wordsAll <- listNotForgottenWordsByLang langNameFrom
  wordsEvolved <- listEvolvedWordsByLangFromAndTo langNameFrom langNameTo
  return $ wordsAll \\ wordsEvolved

listEvolvedWordsByLangFromAndTo :: (MonadIO m) => LanguageName -> LanguageName -> AppT m [Entity Word]
listEvolvedWordsByLangFromAndTo langNameFrom langNameTo =
  select $
  from $ \(word `InnerJoin` langFrom `InnerJoin` wordOrgFrom `InnerJoin` wordOrg `InnerJoin` wordTo `InnerJoin` langTo) -> do
      on (wordTo ^. WordLangId ==. langTo ^. LanguageId)
      on (wordOrg ^. WordOriginWordId ==. wordTo ^. WordId)
      on (wordOrgFrom ^. WordOriginFromOriginId ==. wordOrg ^. WordOriginId)
      on (word ^. WordId ==. wordOrgFrom ^. WordOriginFromWordFromId)
      on (word ^. WordLangId ==. langFrom ^. LanguageId)
      where_ (langFrom ^. LanguageLname ==. val langNameFrom &&.
              langTo ^. LanguageLname ==. val langNameTo)
      groupBy (word ^. WordId)
      return word

listEvolvedWordsToKeysByWordFromAndTo :: (MonadIO m) => Entity Word -> LanguageName -> AppT m [Entity Word]
listEvolvedWordsToKeysByWordFromAndTo wordFrom langNameTo =
   select $
   from $ \(wordOrgFrom `InnerJoin` wordOrg `InnerJoin` wordTo `InnerJoin` langTo) -> do
      on (wordTo ^. WordLangId ==. langTo ^. LanguageId)
      on (wordOrg ^. WordOriginWordId ==. wordTo ^. WordId)
      on (wordOrgFrom ^. WordOriginFromOriginId ==. wordOrg ^. WordOriginId)
      on (val (entityKey wordFrom) ==. wordOrgFrom ^. WordOriginFromWordFromId)
      where_ (langTo ^. LanguageLname ==. val langNameTo)
      groupBy (wordTo ^. WordId)
      return wordTo

deleteEvolvedWordsByLangFromAndTo :: (MonadIO m) => LanguageName -> LanguageName -> AppT m ()
deleteEvolvedWordsByLangFromAndTo langNameFrom langNameTo =
  delete $
  from $ \((word `InnerJoin` langFrom) `InnerJoin` (wordOrgFrom `InnerJoin` wordOrg `InnerJoin` wordTo `InnerJoin` langTo)) -> do
      on (wordTo ^. WordLangId ==. langTo ^. LanguageId)
      on (wordOrg ^. WordOriginWordId ==. wordTo ^. WordId)
      on (wordOrgFrom ^. WordOriginFromOriginId ==. wordOrg ^. WordOriginId)
      on (word ^. WordId ==. wordOrgFrom ^. WordOriginFromWordFromId)
      on (word ^. WordLangId ==. langFrom ^. LanguageId)
      where_ (langFrom ^. LanguageLname ==. val langNameFrom &&.
              langTo ^. LanguageLname !=. val langNameTo)

getAllWordOrigins :: (MonadIO m) => Entity Word -> AppT m [(Entity Word, Entity Language)]
getAllWordOrigins word = do
  mOrigin <- getWordOrigin word
  case mOrigin of
    (Just origin) -> do
      wordsAndLangs <- getWordFromWordLang origin
      otherWordsAndLangs <- mapM getOthersWordLangs wordsAndLangs
      return $ mconcat [wordsAndLangs, mconcat otherWordsAndLangs]
    _             -> 
      return []
  where 
    getOthersWordLangs (eWord, _) = getAllWordOrigins eWord


getWordOrigin :: (MonadIO m) => Entity Word ->  AppT m (Maybe (Entity WordOrigin))
getWordOrigin word = getBy $ EvolveLawWordIdUnq (entityKey word)

getWordFromWordLang :: (MonadIO m) => Entity WordOrigin -> AppT m [(Entity Word, Entity Language)]
getWordFromWordLang wordOrigin =
  select $
  from $ \(wordOriginFrom `InnerJoin` word `InnerJoin` lang) -> do
    on (lang ^. LanguageId ==. word ^. WordLangId)
    on (word ^. WordId ==. wordOriginFrom ^. WordOriginFromWordFromId)
    where_ (wordOriginFrom ^. WordOriginFromOriginId ==. val (entityKey wordOrigin))
    return (word, lang)

-- ???
-- insertWord :: (MonadIO m, MonadLogger m) =>
--      WordText
--   -> PartOfSpeech
--   -> LanguageName
--   -> LanguageName
--   -> AppT m (Maybe (Key Word))
-- insertWord wordText posWord fromLang toLang = do
--     mLangFrom <- getLang fromLang
--     mLangTo <- getLang toLang
--     case (mLangFrom, mLangTo) of
--       (Nothing, _) ->  do
--         logErrorNS "addTranslationFromTo" "There is no such lang from in the database"
--         return Nothing
--       (_, Nothing) ->  do
--         logErrorNS "addTranslationFromTo" "There is no such lang to in the database"
--         return Nothing
--       (Just langFrom, Just langTo) -> do
--         mWord <- getBy $ WordWordPosLangIdUnq wordText posWord (entityKey langFrom)
--         case mWord of
--           Nothing -> do
--             logErrorNS "addTranslationFromTo" "There is no such lang from in the database"
--             return Nothing
--           Just word -> insertUnique $ Word ((wordWord . entityVal) word) (entityKey langTo) ((wordPartOfSpeech . entityVal) word) False
--   where
--     getLang = getBy . LanguageNameUnq
