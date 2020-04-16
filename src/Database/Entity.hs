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

import           ClassyPrelude       (Bool, Show, Text, mconcat, show, ($))
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
    forgotten Bool
    WordWordPosLangIdUnq word partOfSpeech langId
Translation sql=translation_tbl
    fromWordId WordId
    toLangId LanguageId
    toWordId WordId Maybe
    comment Text Maybe
    altTranslation Text Maybe
    deriving Show
WordOrigin sql=word_origin_tbl
    wordId WordId
    comment Text Maybe
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
    soundRegexFrom Text
    soundTo Text
    -- priority Int64
    deriving Show
|]

instance Show Language where
    show l = mconcat [ show $ languageLname l]
