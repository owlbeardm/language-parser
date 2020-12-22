module Database.Word 
  (
  addWordByLangName,
  findWordById, 
  findWordsByAncestorText,
  findWordsByText,
  getAllWordOrigins, 
  getEvolvedWord, 
  listCombinedWordsByLangFromAndTo,
  listDerivatedWordsByLangFromAndTo,
  listEvolvedWordsByLangFromAndTo, 
  listMigratedWordsByLangFromAndTo,
  listNotEvolvedWordsByLangFromAndTo,
  listWordsByLang,
  listWordsByLangAndSound,
  listWordsInfo,
  ) where

import           ClassyPrelude        hiding (Word, delete, groupBy, isNothing,
                                       on, (\\))
import           Control.Monad.Logger
import           Data.List            ((\\))
import           Database.Base
import           Database.Entity
import           Database.Esqueleto

findWordById_ :: (MonadIO m) => Int64 -> AppT m [Entity Word]
findWordById_ i = select $ from $ \word -> do
   where_ (word ^. WordId ==. val (toSqlKey i))
   return word

findWordById :: (MonadIO m) => Int64 -> AppT m (Maybe (Entity Word))
findWordById i = do
  wordsById <- findWordById_ i
  return $ if null wordsById then Nothing else Just ((head . impureNonNull) wordsById)

addWord :: (MonadIO m) => WordText -> Key Language -> PartOfSpeech -> Bool ->  AppT m (Key Word)
addWord word langKey pos forgotten  = insert $ Word word langKey pos forgotten


-- |The 'addWordByLangName' function inserts new word for language.
--
-- >>> runSQLAction $ addWordByLangName "kibil" Noun Khuzdûl
-- Just (WordKey {unWordKey = SqlBackendKey {unSqlBackendKey = 19960}})
addWordByLangName :: (MonadIO m, MonadLogger m) =>  
                  WordText     -- ^ 'Text' of new word, must be unique with 'PartOfSpeech' and 'LanguageName' combined.
                  -> PartOfSpeech     -- ^ 'PartOfSpeech'
                  -> LanguageName     -- ^ 'LanguageName'
                  ->  AppT m (Maybe (Key Word))     -- ^ 'Key' 'Word' if inserted sucsessfully
addWordByLangName  word pos langName = do
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

findWordsByAncestorText :: (MonadIO m) => WordText-> AppT m  [Entity Word]
findWordsByAncestorText textAnc =
    select $
    from $ \(word `InnerJoin` wordOrig `InnerJoin` wordOrigFrom `InnerJoin` wordAnc) -> do
      on (wordOrigFrom ^. WordOriginFromWordFromId ==. wordAnc ^. WordId)
      on (wordOrig ^. WordOriginId ==. wordOrigFrom ^. WordOriginFromOriginId)
      on (word ^. WordId ==. wordOrig ^. WordOriginWordId)
      where_ (wordAnc ^. WordWord ==. val textAnc)
      return word

listWordsByLang :: (MonadIO m) => LanguageName -> AppT m [Entity Word]
listWordsByLang langName = select $ from $ \(word,lang) -> do
      where_ (word ^. WordLangId ==. lang ^. LanguageId &&.
              lang ^. LanguageLname ==. val langName )
      orderBy [asc (word ^. WordLangId), asc (word ^. WordWord)]
      return word

listWordsByLangAndSound :: (MonadIO m) => LanguageName -> Text -> AppT m [Entity Word]
listWordsByLangAndSound langName sound = select $ from $ \(word,lang) -> do
      where_ (word ^. WordLangId ==. lang ^. LanguageId &&.
              lang ^. LanguageLname ==. val langName &&.
              (word ^. WordWord `like` val (mconcat ["%", sound, "%"])))
      orderBy [asc (word ^. WordLangId), asc (word ^. WordWord)]
      return word

listNotForgottenWordsByLang :: (MonadIO m) => LanguageName -> AppT m [Entity Word]
listNotForgottenWordsByLang langName = select $ from $ \(word,lang) -> do
      where_ (word ^. WordLangId ==. lang ^. LanguageId &&.
              word ^. WordForgotten ==. val False &&.
              lang ^. LanguageLname ==. val langName )
      return word

listNotEvolvedWordsByLangFromAndTo :: (MonadIO m) => LanguageName -> LanguageName -> AppT m [Entity Word]
listNotEvolvedWordsByLangFromAndTo langNameFrom langNameTo = do
  wordsAll <- listNotForgottenWordsByLang langNameFrom
  wordsEvolved <- listEvolvedWordsByLangFromAndTo langNameFrom langNameTo
  return $ wordsAll \\ wordsEvolved


listCombinedWordsByLangFromAndTo :: (MonadIO m) => LanguageName -> LanguageName -> AppT m [Entity Word]
listCombinedWordsByLangFromAndTo langNameFrom langNameTo =
  select $
  from $ \(word `InnerJoin` langFrom `InnerJoin` wordOrgFrom `InnerJoin` wordOrg `InnerJoin` wordTo `InnerJoin` langTo) -> do
      on (wordTo ^. WordLangId ==. langTo ^. LanguageId)
      on (wordOrg ^. WordOriginWordId ==. wordTo ^. WordId)
      on (wordOrgFrom ^. WordOriginFromOriginId ==. wordOrg ^. WordOriginId)
      on (word ^. WordId ==. wordOrgFrom ^. WordOriginFromWordFromId)
      on (word ^. WordLangId ==. langFrom ^. LanguageId)
      where_ (langFrom ^. LanguageLname ==. val langNameFrom &&.
              langTo ^. LanguageLname ==. val langNameTo &&.
              wordOrg ^. WordOriginCombinedYn ==. val True)
      groupBy (word ^. WordId)
      return word

listMigratedWordsByLangFromAndTo :: (MonadIO m) => LanguageName -> LanguageName -> AppT m [Entity Word]
listMigratedWordsByLangFromAndTo langNameFrom langNameTo =
  select $
  from $ \(word `InnerJoin` langFrom `InnerJoin` wordOrgFrom `InnerJoin` wordOrg `InnerJoin` wordTo `InnerJoin` langTo) -> do
      on (wordTo ^. WordLangId ==. langTo ^. LanguageId)
      on (wordOrg ^. WordOriginWordId ==. wordTo ^. WordId)
      on (wordOrgFrom ^. WordOriginFromOriginId ==. wordOrg ^. WordOriginId)
      on (word ^. WordId ==. wordOrgFrom ^. WordOriginFromWordFromId)
      on (word ^. WordLangId ==. langFrom ^. LanguageId)
      where_ (langFrom ^. LanguageLname ==. val langNameFrom &&.
              langTo ^. LanguageLname ==. val langNameTo &&.
              wordOrg ^. WordOriginMigratedYn ==. val True)
      groupBy (word ^. WordId)
      return word


listDerivatedWordsByLangFromAndTo :: (MonadIO m) => LanguageName -> LanguageName -> AppT m [Entity Word]
listDerivatedWordsByLangFromAndTo langNameFrom langNameTo =
  select $
  from $ \(word `InnerJoin` langFrom `InnerJoin` wordOrgFrom `InnerJoin` wordOrg `InnerJoin` wordTo `InnerJoin` langTo) -> do
      on (wordTo ^. WordLangId ==. langTo ^. LanguageId)
      on (wordOrg ^. WordOriginWordId ==. wordTo ^. WordId)
      on (wordOrgFrom ^. WordOriginFromOriginId ==. wordOrg ^. WordOriginId)
      on (word ^. WordId ==. wordOrgFrom ^. WordOriginFromWordFromId)
      on (word ^. WordLangId ==. langFrom ^. LanguageId)
      where_ (langFrom ^. LanguageLname ==. val langNameFrom &&.
              langTo ^. LanguageLname ==. val langNameTo &&.
              wordOrg ^. WordOriginDerivatedYn ==. val True)
      groupBy (word ^. WordId)
      return word

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
              langTo ^. LanguageLname ==. val langNameTo &&.
              wordOrg ^. WordOriginEvolvedYn ==. val True)
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
      where_ (langTo ^. LanguageLname ==. val langNameTo &&.
              wordOrg ^. WordOriginEvolvedYn ==. val True)
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
              langTo ^. LanguageLname !=. val langNameTo &&.
              wordOrg ^. WordOriginEvolvedYn ==. val True)

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

getEvolvedWord :: (MonadIO m) => LanguageName -> Entity Word -> AppT m [Entity Word]
getEvolvedWord langNameTo wordFrom =
  select $
  from $ \(wordOrgFrom `InnerJoin` wordOrg `InnerJoin` wordTo `InnerJoin` langTo) -> do
    on (wordTo ^. WordLangId ==. langTo ^. LanguageId)
    on (wordOrg ^. WordOriginWordId ==. wordTo ^. WordId)
    on (wordOrgFrom ^. WordOriginFromOriginId ==. wordOrg ^. WordOriginId)
    where_ (langTo ^. LanguageLname ==. val langNameTo &&.
            val (entityKey wordFrom) ==. wordOrgFrom ^. WordOriginFromWordFromId &&.
              wordOrg ^. WordOriginEvolvedYn ==. val True)
    return wordTo

listWordsInfo :: (MonadIO m) => LanguageName -> ([Entity Word] -> a) -> AppT m a
listWordsInfo langName infoGetter = do
  wrds <- listWordsByLang langName
  return $ infoGetter wrds