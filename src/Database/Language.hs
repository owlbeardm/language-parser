module Database.Language where

import           ClassyPrelude      hiding (Word, keys, on, words)
import           Control.Monad      (zipWithM)
import qualified Data.Text          as T
import           Database.Base
import           Database.Entity
import           Database.Esqueleto
import           Database.Word
import qualified Text.Regex         as R

findLangById :: (MonadIO m) => Int64 -> AppT m [Entity Language]
findLangById i = select $ from $ \lang -> do
   where_ (lang ^. LanguageId ==. val (toSqlKey i))
   return lang

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

listEvolveLawsByLangs :: (MonadIO m) => LanguageName -> LanguageName -> AppT m [Entity EvolveLaw]
listEvolveLawsByLangs langNameFrom langNameTo = select $ from $ \(evolveLaw,langFrom,langTo) -> do
      where_ (evolveLaw ^. EvolveLawLangFromId ==. langFrom ^. LanguageId &&.
              langFrom ^. LanguageLname ==. val langNameFrom &&.
              evolveLaw ^. EvolveLawLangToId ==. langTo ^. LanguageId &&.
              langTo ^. LanguageLname ==. val langNameTo)
              --order by prior
      return evolveLaw

evolveWordText :: WordText -> [EvolveLaw] -> WordText
evolveWordText = foldl' changeWord

-- |The 'square' function squares an integer.
changeWord :: WordText -> EvolveLaw -> WordText
changeWord wordText law = T.pack $ R.subRegex regex word soundTo
   where
      regex = (R.mkRegex . T.unpack . evolveLawSoundRegexFrom) law
      soundTo = (T.unpack . evolveLawSoundTo) law
      word = T.unpack wordText

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

evolveLangWithAll :: (MonadIO m) => LanguageName -> [LanguageName] -> AppT m [(Int, LanguageName, LanguageName)]
evolveLangWithAll lang langs = do
   keys <- mapM (evolveLang lang) langs
   return $ catMaybes keys

evolveAllLangWithAll :: (MonadIO m) => AppT m [(Int, LanguageName, LanguageName)]
evolveAllLangWithAll = do
   langs <- listLangs
   let langNames = map (languageLname . entityVal) langs
   result <- zipWithM evolveLangWithAll langNames (repeat langNames)
   return $ mconcat result
