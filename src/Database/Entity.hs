{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Database.Entity where

import           ClassyPrelude       (Bool, Show, Text, mconcat, show, ($), Maybe, MonadIO, Eq)
import           Data.Int
import           Database.Base
import           Database.Persist.TH
import           Database.Esqueleto

type WordText   = Text
type Comment    = Text
type SoundRegex = Text
type Sound      = Text
type Priority   = Int64

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Language sql=language_tbl
    lname LanguageName sql=name
    LanguageNameUnq lname
Word sql=word_tbl
    word WordText
    langId LanguageId
    partOfSpeech PartOfSpeech
    forgotten Bool
    WordWordPosLangIdUnq word partOfSpeech langId
    deriving Eq
Translation sql=translation_tbl
    fromWordId WordId
    toLangId LanguageId
    toWordId WordId Maybe
    comment Comment Maybe
    altTranslation Text Maybe
    deriving Show
WordOrigin sql=word_origin_tbl
    wordId WordId
    comment Comment Maybe
    evolvedYn Bool
    migratedYn Bool
    combinedYn Bool
    derivatedYn Bool
    EvolveLawWordIdUnq wordId
    deriving Show
WordOriginFrom sql=word_origin_from_tbl
    wordFromId WordId
    originId WordOriginId
    deriving Show
EvolveLaw sql=evolve_law_tbl
    langFromId LanguageId
    langToId LanguageId
    soundRegexFrom SoundRegex
    soundTo Sound
    priority Priority
    deriving Show
|]

instance Show Language where
    show l = mconcat [ show $ languageLname l]

findLangByName :: (MonadIO m) => LanguageName -> AppT m  (Maybe (Entity Language))
findLangByName name =  getBy $ LanguageNameUnq name

