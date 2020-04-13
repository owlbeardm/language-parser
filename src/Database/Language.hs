module Database.Language where

import           ClassyPrelude             hiding (Word)
-- import           Control.Exception         (throw)
-- import           Control.Monad.Trans.Maybe
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

insertEvolvedWord :: Text -> PartOfSpeech -> Key Word -> Key Language -> IO (Key Word)
insertEvolvedWord textToAdd pos wfKey langToKey = runSQLAction $ do
   wordToKey <- insert $ Word textToAdd langToKey pos
   wordOriginKey <- insert $ WordOrigin wordToKey Nothing True False False False
   _ <- insert $ WordOriginFrom wfKey wordOriginKey
   return wordToKey

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


