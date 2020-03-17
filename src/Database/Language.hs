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



-- addLang :: LanguageName -> IO ()
-- addLang name = runSQLAction . void . insert $ Language name



-- addTranslationFromTo ::
--      Text
--   -> PartOfSpeech
--   -> LanguageName
--   -> Text
--   -> PartOfSpeech
--   -> LanguageName
--   -> Maybe Text
--   -> IO ()
-- addTranslationFromTo fromWord fromPos fromLang toWord toPos toLang mComment =
--   runSQLAction $ do
--     mLangFrom <- getLang fromLang
--     mLangTo <- getLang toLang
--     case (mLangFrom, mLangTo) of
--       (Nothing, _) ->
--         logErrorNS logLabel "There is no such lang to in the database"
--       (_, Nothing) ->
--         logErrorNS logLabel "There is no such lang from in the database"
--       (Just langFrom, Just langTo) -> do
--         mWordFrom <-
--           getBy $ WordWordPosLangIdUnq fromWord fromPos (entityKey langFrom)
--         mWordTo <- getBy $ WordWordPosLangIdUnq toWord toPos (entityKey langTo)
--         case (mWordFrom, mWordTo) of
--           (Nothing, _) ->
--             logErrorNS logLabel "There is no such word to in the database"
--           (_, Nothing) ->
--             logErrorNS logLabel "There is no such word from in the database"
--           (Just wordFrom, Just wordTo) -> do
--             id <-
--               insert $
--               Translation
--                 (entityKey wordFrom)
--                 (entityKey langTo)
--                 (Just (entityKey wordTo))
--                 mComment
--                 Nothing
--             liftIO $ print id
--   where
--     getLang = getBy . LanguageNameUnq
--     logLabel = "addTranslationFromTo"
