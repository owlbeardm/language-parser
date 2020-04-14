module Database.Language where

import           ClassyPrelude        hiding (Word)
-- import           Control.Exception         (throw)
-- import           Control.Monad.Trans.Maybe
import           Control.Monad.Logger
import           Database.Base
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

evolvedWord :: Text -> PartOfSpeech -> LanguageName -> IO (Maybe (Key Word))
evolvedWord word pos langName = runSQLAction $ do
   mLang <- getLang langName
   case mLang of
      Nothing ->  do
        logErrorNS "evolvedWord" "There is no such lang from in the database"
        return Nothing
      Just lang -> do
        mWordFrom <- getBy $ WordWordPosLangIdUnq word pos (entityKey lang)
        case mWordFrom of
            Nothing ->  do
               logErrorNS "evolvedWord" "There is no such word from in the database"
               return Nothing
            Just wordFrom -> do
               key <- insertEvolvedWord (wordWord (entityVal wordFrom)) (wordPartOfSpeech (entityVal wordFrom)) (entityKey wordFrom) (entityKey lang)
               return $ Just key
   where
    getLang = getBy . LanguageNameUnq


getAll words by lang
for each word 
   if no evolved word than evolve
      for each word change text
      for each word new word
      
-- getWord' :: (MonadIO m) => LanguageName -> Text -> PartOfSpeech -> AppT m  (Maybe (Entity Word))
-- getWord' langName word pos = do
--    lang <- getLangByName langName
--    maybe
--        (throw ProtoMaterial)
--        (\l -> getBy $ WordWordPosLangIdUnq word pos (entityKey l))
--        lang



-- mauth <- runDb $ do
--           ma <- runMaybeT $ do
--                    valid <- ...
--           case ma of
--             Just _ -> return ma
--             Nothing -> liftIO $ throwIO MyException


-- type FullTranslation = (Translation, Word, Language, Language, Maybe Word)


-- mainUnq :: Text -> PartOfSpeech -> LanguageName -> IO ()
-- mainUnq word pos langName =
--   runSQLAction $ do
--     lang <- getBy $ LanguageNameUnq langName
--     case lang of
--       Just l -> do
--         mWord <- getBy $ WordWordPosLangIdUnq word pos (entityKey l)
--         liftIO $ mapM_ (putStrLn . tshow . entityVal) mWord
--       Nothing -> liftIO $ print lang

-- mainT :: IO ()
-- mainT =
--   runSQLAction $ do
--     trans <- select $ from $ \tran -> do return tran
--     liftIO $ mapM_ (print . tshow . entityVal) (trans :: [Entity Translation])


