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

import           ClassyPrelude       (Show, Text, mconcat, show, unpack, (>), length,
                                      ($))
import           Database.Base
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Language sql=language_tbl
    lname LanguageName sql=name
    LanguageNameUnq lname
Word sql=word_tbl
    word Text
    langId LanguageId
    partOfSpeech PartOfSpeech
    WordWordPosLangIdUnq word partOfSpeech langId
Translation sql=translation_tbl
    fromWordId WordId
    toLangId LanguageId
    toWordId WordId Maybe
    comment Text Maybe
    altTranslation Text Maybe
    deriving Show
|]

instance Show Language where
    show l = mconcat [ show $ languageLname l]

instance Show Word where
    show w = mconcat [ unpack $ wordWord w, if ((length (wordWord w)) > 7) then "\t" else "\t\t", "'", show $ wordPartOfSpeech w, "'"]

