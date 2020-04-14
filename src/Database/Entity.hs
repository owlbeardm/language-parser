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

import           ClassyPrelude       (Show, Text, length, mconcat, show, unpack, Bool,
                                      ($), (>))
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
    langFromId
    langToId
    soundLawId
    priority
SoundLaw sql=sound_law_tbl
    ruleFrom
    ruleTo
|]

instance Show Language where
    show l = mconcat [ show $ languageLname l]
