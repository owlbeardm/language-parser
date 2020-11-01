module Database.Language where

import           ClassyPrelude      hiding (Word, keys, on, words)
import           Control.Monad      (zipWithM)
import qualified Data.Text          as T
import           Database.Base
import           Database.Entity
import           Database.Esqueleto
import           Database.Word
-- import qualified Text.Regex         as R
-- import           Text.Regex.Base
import           Text.Regex.PCRE

findLangById_ :: (MonadIO m) => Int64 -> AppT m [Entity Language]
findLangById_ i = select $ from $ \lang -> do
   where_ (lang ^. LanguageId ==. val (toSqlKey i))
   return lang

findLangById :: (MonadIO m) => Int64 -> AppT m (Maybe (Entity Language))
findLangById i = do
  langsById <- findLangById_ i
  return $ if null langsById then Nothing else Just ((head . impureNonNull) langsById)

findLangByKey :: (MonadIO m) => Key Language -> AppT m [Entity Language]
findLangByKey k = select $ from $ \lang -> do
   where_ (lang ^. LanguageId ==. val k)
   return lang

listLangs :: (MonadIO m) => AppT m [Entity Language]
listLangs = select $ from $ \lang -> return lang

addLang :: (MonadIO m) => LanguageName -> AppT m  (Key Language)
addLang name = insert $ Language name

insertEvolvedWord :: (MonadIO m) => WordText -> PartOfSpeech -> Key Word -> Key Language -> AppT m (Key Word)
insertEvolvedWord textToAdd pos wfKey langToKey = do
   wordToKey <- insert $ Word textToAdd langToKey pos False
   wordOriginKey <- insert $ WordOrigin wordToKey Nothing True False False False
   _ <- insert $ WordOriginFrom wfKey wordOriginKey
   return wordToKey

insertCombinedWord :: (MonadIO m) => WordText -> PartOfSpeech -> Key Language -> [Key Word] -> AppT m (Key Word)
insertCombinedWord textToAdd pos langToKey wKeys = do
   wordToKey <- insert $ Word textToAdd langToKey pos False
   wordOriginKey <- insert $ WordOrigin wordToKey Nothing False False True False
   _ <- mapM (\wk -> insert $ WordOriginFrom wk wordOriginKey) wKeys
   return wordToKey

listEvolveLawsByLangs :: (MonadIO m) => LanguageName -> LanguageName -> AppT m [Entity EvolveLaw]
listEvolveLawsByLangs langNameFrom langNameTo = select $ from $ \(evolveLaw,langFrom,langTo) -> do
      where_ (evolveLaw ^. EvolveLawLangFromId ==. langFrom ^. LanguageId &&.
              langFrom ^. LanguageLname ==. val langNameFrom &&.
              evolveLaw ^. EvolveLawLangToId ==. langTo ^. LanguageId &&.
              langTo ^. LanguageLname ==. val langNameTo)
      orderBy [asc (evolveLaw ^. EvolveLawId)]
              --order by prior
      return evolveLaw

evolveWordText :: WordText -> [EvolveLaw] -> WordText
evolveWordText = foldl' changeWord

-- |The 'square' function squares an integer.
changeWord :: WordText -> EvolveLaw -> WordText
changeWord wordText law = T.pack $ replaceAll regex soundTo word
   where
      regex = (T.unpack . evolveLawSoundRegexFrom) law
      soundTo = (T.unpack . evolveLawSoundTo) law
      word = T.unpack wordText

replaceAll :: String -> String -> String -> String
replaceAll _ _ [] = ""
replaceAll rexp target source  =
    let res :: (String,String,String,[String])
        res = (source =~ rexp)
        rpAll = replaceAll rexp target
    in if null (second4 res)
        then source
        else first4 res ++ target ++ rpAll (third4 res)
    where
        first4 (a,_,_,_) = a
        second4 (_,b,_,_) = b
        third4 (_,_,c,_) = c

combineWord :: (MonadIO m) => WordText -> PartOfSpeech -> LanguageName -> [Int64] -> AppT m (Maybe (Key Word))
combineWord text pos langN wids = do
   mLang <- findLangByName langN
   case mLang of
         Nothing -> return Nothing
         (Just langE) -> do
            wds <- mapM findWordById wids
            if Nothing `elem` wds
               then return Nothing
               else do
                  cmbW <- insertCombinedWord text pos (entityKey langE) (map entityKey (catMaybes wds))
                  return $ Just cmbW

evolvedWord :: (MonadIO m) => [EvolveLaw] -> Entity Word -> Key Language -> AppT m (Key Word)
evolvedWord laws eWordFrom = insertEvolvedWord evolvedText ((wordPartOfSpeech . entityVal) eWordFrom) (entityKey eWordFrom)
   where
      evolvedText = evolveWordText ((wordWord . entityVal) eWordFrom) laws

evolveLang :: (MonadIO m) => LanguageName -> LanguageName -> AppT m (Maybe (Int, LanguageName, LanguageName))
evolveLang langNameFrom langNameTo
   | langNameFrom == langNameTo
      = return Nothing
   | otherwise
      = do
      mLangTo <- findLangByName langNameTo
      case mLangTo of
         Nothing -> return Nothing
         (Just langTo) -> do
            words <- listNotEvolvedWordsByLangFromAndTo langNameFrom langNameTo
            evolveLaws <- listEvolveLawsByLangs langNameFrom langNameTo
            if null evolveLaws
               then return Nothing
               else (do
                  keys <- mapM (\x -> evolvedWord (map entityVal evolveLaws) x (entityKey langTo)) words
                  return $ Just (length keys, langNameFrom, langNameTo))

reEvolveLang :: (MonadIO m) => LanguageName -> LanguageName -> AppT m (Maybe (Int, LanguageName, LanguageName))
reEvolveLang langNameFrom langNameTo
   | langNameFrom == langNameTo
      = return Nothing
   | otherwise
      = do
         evolveLaws <- listEvolveLawsByLangs langNameFrom langNameTo
         words <- listEvolvedWordsByLangFromAndTo langNameFrom langNameTo
         wordsToUpdate <- mapM (getEvolvedWord langNameTo) words
         let wordsTupels = (unzip . mconcat) $ zipWordsWordsTo words wordsToUpdate
         result <- mapM updateWord $ uncurry makeWordsWithNewText wordsTupels evolveLaws
         return $ Just ((sum . map fromIntegral) result, langNameFrom, langNameTo)
   where
      zipWordsWordsTo = zipWith (curry cycleWords)
      cycleWords (word, wordsTo) = zip (repeat word) wordsTo
      makeWordsWithNewText wordFrom words evolveLaws = zip words $ makeNewText evolveLaws wordFrom
      makeNewText evolveLaws = map (\x -> evolveWordText (wordWord (entityVal x)) (map entityVal evolveLaws))

updateWord :: (MonadIO m) => (Entity Word, Text) -> AppT m Int64
updateWord (enWord, text) =
   updateCount $
   \word -> do
     set word [ WordWord =. val text ]
     where_ (val (entityKey enWord) ==. word ^. WordId &&.
             val text !=. word ^. WordWord)

-- evolveLang :: (MonadIO m) => LanguageName -> LanguageName -> AppT m (Maybe [Key Word])
-- evolveLang langNameFrom langNameTo

-- reEvolveLang :: (MonadIO m) =>

doLangWithAll :: (MonadIO m) => (LanguageName -> LanguageName -> AppT m (Maybe (Int, LanguageName, LanguageName))) -> LanguageName -> [LanguageName] -> AppT m [(Int, LanguageName, LanguageName)]
doLangWithAll doLang lang langs  = do
   keys <- mapM (doLang lang) langs
   return $ catMaybes keys

doAllLangWithAll :: (MonadIO m) => (LanguageName -> LanguageName -> AppT m (Maybe (Int, LanguageName, LanguageName))) -> AppT m [(Int, LanguageName, LanguageName)]
doAllLangWithAll doLang = do
   langs <- listLangs
   let langNames = map (languageLname . entityVal) langs
   result <- zipWithM (doLangWithAll doLang) langNames (repeat langNames)
   return $ mconcat result

evolveAllLangWithAll :: (MonadIO m) => AppT m [(Int, LanguageName, LanguageName)]
evolveAllLangWithAll = doAllLangWithAll evolveLang

reEvolveAllLangWithAll :: (MonadIO m) => AppT m [(Int, LanguageName, LanguageName)]
reEvolveAllLangWithAll = doAllLangWithAll reEvolveLang


traceWordEvolve :: (MonadIO m) => WordText -> [LanguageName] -> AppT m [WordText]
traceWordEvolve _   []         = return []
traceWordEvolve wrd [_]        = return [wrd]
traceWordEvolve wrd (l1:l2:xs) = do
   ewrd <- evolvedWrd l1 l2 wrd
   left <- traceWordEvolve ewrd (l2:xs)
   return (wrd:left)
   where
      evolvedWrd ll1 ll2 wwrd = do
         laws <- listEvolveLawsByLangs ll1 ll2
         return $ evolveWordText wwrd (map entityVal laws)
