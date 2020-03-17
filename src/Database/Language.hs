module Database.Language where

import           ClassyPrelude
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


