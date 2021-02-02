module Database.Language
  ( addLang
  , doAllLangWithAll
  , evolveLang
  , evolveLangsTo
  , findLangByKey
  , getWordsEvolved
  , getWordsToEvolve
  , listEvolveLawsByLangs
  , listLangs
  , newWordWithOrigin
  , reEvolveLang
  , traceWordEvolve
  ) where

import           ClassyPrelude      hiding (Word, groupBy, keys, on, words)
import           Control.Monad      (zipWithM)
import           Database.Base
import           Database.Entity
import           Database.Esqueleto
import           Database.Word
import           Language.Language

findLangById_ :: (MonadIO m) => Int64 -> AppT m [Entity Language]
findLangById_ i =
  select $
  from $ \lang -> do
    where_ (lang ^. LanguageId ==. val (toSqlKey i))
    return lang

findLangById :: (MonadIO m) => Int64 -> AppT m (Maybe (Entity Language))
findLangById i = do
  langsById <- findLangById_ i
  return $
    if null langsById
      then Nothing
      else Just ((head . impureNonNull) langsById)

findLangByKey :: (MonadIO m) => Key Language -> AppT m [Entity Language]
findLangByKey k =
  select $
  from $ \lang -> do
    where_ (lang ^. LanguageId ==. val k)
    return lang

listLangs :: (MonadIO m) => AppT m [Entity Language]
listLangs = select $ from $ \lang -> return lang

-- |The 'addLang' function inserts new language.
--
-- >>> runSQLAction $ addLang 'Khuzdûl'
-- LanguageKey {unLanguageKey = SqlBackendKey {unSqlBackendKey = 19950}}
addLang ::
     (MonadIO m)
  => LanguageName -- ^ 'LanguageName'
  -> AppT m (Key Language) -- ^ 'Key' 'Language' if inserted succsesfully
addLang name = insert $ Language name

-- insertEvolvedWord ::
--      (MonadIO m)
--   => WordText
--   -> PartOfSpeech
--   -> Key Word
--   -> Key Language
--   -> AppT m (Key Word)
-- insertEvolvedWord textToAdd pos wfKey langToKey = do
--   wordToKey <- insert $ Word textToAdd langToKey pos False
--   wordOriginKey <- insert $ WordOrigin wordToKey Nothing True False False False
--   _ <- insert $ WordOriginFrom wfKey wordOriginKey
--   return wordToKey
insertWordWithOrigin ::
     (MonadIO m)
  => WordText
  -> PartOfSpeech
  -> Key Language
  -> Bool
  -> WordOriginType
  -> [Key Word]
  -> AppT m (Key Word)
insertWordWithOrigin textToAdd pos langToKey forgotten woType wKeys = do
  wordToKey <- insert $ Word textToAdd langToKey pos forgotten
  wordOriginKey <-
    insert $
    WordOrigin
      wordToKey
      Nothing
      (woType == Evolved)
      (woType == Migrated)
      (woType == Combined)
      (woType == Derivated)
  _ <- mapM (\wk -> insert $ WordOriginFrom wk wordOriginKey) wKeys
  return wordToKey

listEvolveLawsByLangs ::
     (MonadIO m) => LanguageName -> LanguageName -> AppT m [Entity EvolveLaw]
listEvolveLawsByLangs langNameFrom langNameTo =
  select $
  from $ \(evolveLaw, langFrom, langTo) -> do
    where_
      ((evolveLaw ^. EvolveLawLangFromId ==. langFrom ^. LanguageId) &&.
       (langFrom ^. LanguageLname ==. val langNameFrom) &&.
       (evolveLaw ^. EvolveLawLangToId ==. langTo ^. LanguageId) &&.
       (langTo ^. LanguageLname ==. val langNameTo))
    orderBy [asc (evolveLaw ^. EvolveLawId)]
              --order by prior
    return evolveLaw

-- |The 'combineWord' function inserts result of combining other words from any language.
--
-- @\/ kʷiliati \/ [v.] ( __19240__ ) from 'Queran'@
--
-- >>> runSQLAction $ combineWord "kʷilissa" Noun Queran [19240, 19300]
-- Just (WordKey {unWordKey = SqlBackendKey {unSqlBackendKey = 19310}})
newWordWithOrigin ::
     (MonadIO m)
  => WordText -- ^ Result of combinations. Must be done manually.
  -> PartOfSpeech -- ^ 'PartOfSpeech' of the resulting word
  -> LanguageName -- ^ 'LanguageName' of the resulting word
  -> Bool
  -> WordOriginType
  -> [Int64] -- ^ Word's IDs from which  it's combined.
  -> AppT m (Maybe (Key Word))
newWordWithOrigin text pos langN forgotten woType wids = do
  mLang <- findLangByName langN
  case mLang of
    Nothing -> return Nothing
    (Just langE) -> do
      wds <- mapM findWordById wids
      if Nothing `elem` wds
        then return Nothing
        else do
          cmbW <-
            insertWordWithOrigin
              text
              pos
              (entityKey langE)
              forgotten
              woType
              (map entityKey (catMaybes wds))
          return $ Just cmbW

evolvedWord ::
     (MonadIO m)
  => [EvolveLaw]
  -> Entity Word
  -> Key Language
  -> AppT m (Key Word)
evolvedWord laws eWordFrom kLang =
  insertWordWithOrigin
    evolvedText
    ((wordPartOfSpeech . entityVal) eWordFrom)
    kLang
    False
    Evolved
    [entityKey eWordFrom]
  where
    evolvedText = evolveWordText ((wordWord . entityVal) eWordFrom) laws

evolveLang ::
     (MonadIO m)
  => LanguageName
  -> LanguageName
  -> AppT m (Maybe (Int, LanguageName, LanguageName))
evolveLang langNameFrom langNameTo
  | langNameFrom == langNameTo = return Nothing
  | otherwise = do
    mLangTo <- findLangByName langNameTo
    case mLangTo of
      Nothing -> return Nothing
      (Just langTo) -> do
        words <- listNotEvolvedWordsByLangFromAndTo langNameFrom langNameTo
        evolveLaws <- listEvolveLawsByLangs langNameFrom langNameTo
        if null evolveLaws
          then return Nothing
          else (do keys <-
                     mapM
                       (\x ->
                          evolvedWord
                            (map entityVal evolveLaws)
                            x
                            (entityKey langTo) --
                        )
                       words
                   return $ Just (length keys, langNameFrom, langNameTo))

reEvolveLang ::
     (MonadIO m)
  => LanguageName
  -> LanguageName
  -> AppT m (Maybe (Int, LanguageName, LanguageName))
reEvolveLang langNameFrom langNameTo
  | langNameFrom == langNameTo = return Nothing
  | otherwise = do
    evolveLaws <- listEvolveLawsByLangs langNameFrom langNameTo
    words <- listEvolvedWordsByLangFromAndTo langNameFrom langNameTo
    wordsToUpdate <- mapM (getEvolvedWord langNameTo) words
    let wordsTupels = (unzip . mconcat) $ zipWordsWordsTo words wordsToUpdate
    result <-
      mapM updateWord $ uncurry makeWordsWithNewText wordsTupels evolveLaws
    return $ Just ((sum . map fromIntegral) result, langNameFrom, langNameTo)
  where
    zipWordsWordsTo = zipWith (curry cycleWords)
    cycleWords (word, wordsTo) = zip (repeat word) wordsTo
    makeWordsWithNewText wordFrom words evolveLaws =
      zip words $ makeNewText evolveLaws wordFrom
    makeNewText evolveLaws =
      map
        (\x ->
           evolveWordText (wordWord (entityVal x)) (map entityVal evolveLaws))

updateWord :: (MonadIO m) => (Entity Word, Text) -> AppT m Int64
updateWord (enWord, text) =
  updateCount $ \word -> do
    set word [WordWord =. val text]
    where_
      ((val (entityKey enWord) ==. word ^. WordId) &&.
       (val text !=. word ^. WordWord))

-- evolveLang :: (MonadIO m) => LanguageName -> LanguageName -> AppT m (Maybe [Key Word])
-- evolveLang langNameFrom langNameTo
-- reEvolveLang :: (MonadIO m) =>
doLangWithAll ::
     (MonadIO m)
  => (LanguageName -> LanguageName -> AppT m (Maybe ( Int
                                                    , LanguageName
                                                    , LanguageName)))
  -> LanguageName
  -> [LanguageName]
  -> AppT m [(Int, LanguageName, LanguageName)]
doLangWithAll doLang lang langs = do
  keys <- mapM (doLang lang) langs
  return $ catMaybes keys

doAllLangWithAll ::
     (MonadIO m)
  => (LanguageName -> LanguageName -> AppT m (Maybe ( Int
                                                    , LanguageName
                                                    , LanguageName)))
  -> AppT m [(Int, LanguageName, LanguageName)]
doAllLangWithAll doLang = do
  langs <- listLangs
  let langNames = map (languageLname . entityVal) langs
  result <- zipWithM (doLangWithAll doLang) langNames (repeat langNames)
  return $ mconcat result

evolveAllLangWithAll ::
     (MonadIO m) => AppT m [(Int, LanguageName, LanguageName)]
evolveAllLangWithAll = doAllLangWithAll evolveLang

reEvolveAllLangWithAll ::
     (MonadIO m) => AppT m [(Int, LanguageName, LanguageName)]
reEvolveAllLangWithAll = doAllLangWithAll reEvolveLang

traceWordEvolve ::
     (MonadIO m) => WordText -> [LanguageName] -> AppT m [WordText]
traceWordEvolve _ [] = return []
traceWordEvolve wrd [_] = return [wrd]
traceWordEvolve wrd (l1:l2:xs) = do
  ewrd <- evolvedWrd l1 l2 wrd
  left <- traceWordEvolve ewrd (l2 : xs)
  return (wrd : left)
  where
    evolvedWrd ll1 ll2 wwrd = do
      laws <- listEvolveLawsByLangs ll1 ll2
      return $ evolveWordText wwrd (map entityVal laws)

evolveLangsTo :: (MonadIO m) => LanguageName -> AppT m [Entity Language]
evolveLangsTo langNameFrom =
  select $
  from $ \(langFrom `InnerJoin` evolveLaw `InnerJoin` langTo) -> do
    on (evolveLaw ^. EvolveLawLangToId ==. langTo ^. LanguageId)
    on (evolveLaw ^. EvolveLawLangFromId ==. langFrom ^. LanguageId)
    where_ (langFrom ^. LanguageLname ==. val langNameFrom)
    groupBy (langTo ^. LanguageLname, langTo ^. LanguageId)
    return langTo
    --

getWordsToEvolve ::
     (MonadIO m) => LanguageName -> LanguageName -> AppT m [(Entity Word, Text)]
getWordsToEvolve langNameFrom langNameTo = do
  words <- listNotEvolvedWordsByLangFromAndTo langNameFrom langNameTo
  evolveLaws <- listEvolveLawsByLangs langNameFrom langNameTo
  return (map (wordToWordAndEvolved (map entityVal evolveLaws)) words)
  where
    wordToWordAndEvolved laws word =
      (word, evolveWordText ((wordWord . entityVal) word) laws)

-- TODO:
getWordsEvolved :: (MonadIO m) => LanguageName -> LanguageName -> AppT m [(Entity Word, Text, [Entity Word])]
getWordsEvolved langNameFrom langNameTo = do
  evolveLaws <- listEvolveLawsByLangs langNameFrom langNameTo
  words <- listEvolvedWordsByLangFromAndTo langNameFrom langNameTo
  wordsEvolved <- mapM (getEvolvedWord langNameTo) words
  let wordsTupels = zip words wordsEvolved
  return (map (wordToWordAndEvolved (map entityVal evolveLaws)) wordsTupels)
  where
    wordToWordAndEvolved laws (word, wordEvolved) = (word, evolveWordText ((wordWord . entityVal) word) laws, wordEvolved)
