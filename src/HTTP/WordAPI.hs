{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HTTP.WordAPI
      (
      WordsApi,
      wordServer
      ) where

import           ClassyPrelude          (map, return, ($))
import           Control.Monad.IO.Class (liftIO)
import           Database.Base          (LanguageName (..), runSQLAction)
import           Database.Entity        (Language, Word)
import           Database.Esqueleto     (entityVal)
import           Database.Translation   (WordDescription, WordSource,
                                         WordTranslation,
                                         getFullWordDescription)
import           Database.Word          (listWordsByLang)
import           Servant.API
import           Servant.Server

type WordDescriptionAPI = (Word, [Language], [WordTranslation], [WordSource])

type WordsApi = "words" :>
  (   Get '[JSON] [WordDescriptionAPI]
--   :<|> "all" :> Get '[JSON] [Language]
  )

wordServer :: Server WordsApi
wordServer = fetchLanguagesHandler
--   :<|> fetchLanguagesHandler

fetchLanguagesHandler ::  Handler [WordDescriptionAPI]
fetchLanguagesHandler = do
  words <- liftIO $ runSQLAction $ listWordsByLang langName
  fullDescr <- liftIO $ runSQLAction $ getFullWordDescription words
  return (map toWordDescriptionAPI fullDescr)
  where
    langName = Titan

-- printWordsFrom :: LanguageName -> IO ()
-- printWordsFrom langName = runSQLAction $ do
--      words <- listWordsByLang langName
--      fullDescr <- getFullWordDescription words
--      liftIO $ mapM_ (putStrLn . tshowPretty prettyWordDescription) fullDescr
--      putStr "\n\tTotal: "
--      print $ length words

toWordDescriptionAPI :: WordDescription -> WordDescriptionAPI
toWordDescriptionAPI (a, b, c, d) = (entityVal a, b, c, d)
