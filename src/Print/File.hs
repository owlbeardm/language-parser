module Print.File where

import           ClassyPrelude                             hiding (Word, keys,
                                                            lines, words, (<>))
import           Control.Monad                             (liftM2)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Database.Base
import           Database.Entity
import           Database.Esqueleto
import           Database.Language
import           Database.Word

printLangsGML :: IO ()
printLangsGML = runSQLAction $ do
     langs <- listLangs
     lines <- prettyLangs langs
     writeFile "langs.gml" ((encodeUtf8 . renderStrict . layoutPretty defaultLayoutOptions) lines)
     return ()

prettyLangs :: (MonadIO m) => [Entity Language] -> AppT m (Doc AnsiStyle)
prettyLangs langs = do
     edges <- mapM prettyLangToLangEdge (castProd langs langs)
     return $ vsep ["graph",
                         lbracket,
                         "  " <+> align (vsep $ map prettyLang langs),
                         vsep edges,
                         rbracket
                        ]

castProd :: [Entity Language] -> [Entity Language] -> [(Entity Language, Entity Language)]
castProd = liftM2 (,)

prettyLangToLangEdge :: (MonadIO m) =>  (Entity Language, Entity Language) -> AppT m (Doc AnsiStyle)
prettyLangToLangEdge (langFrom, langTo) = do
     evolved <- listEvolvedWordsByLangFromAndTo (getLangName langFrom) (getLangName langTo)
     combined <- listCombinedWordsByLangFromAndTo (getLangName langFrom) (getLangName langTo)
     let prettyEdgeLL ws = prettyEdge langFrom langTo ((not . null) ws)
     return $ vsep [
          prettyEdgeLL evolved "\"standard\"",
          prettyEdgeLL combined "\"dotted\""
          ]
     where
          getLangName = languageLname . entityVal


prettyEdge :: Entity Language -> Entity Language -> Bool -> Text -> Doc AnsiStyle
prettyEdge _     _     False _       = mempty
prettyEdge langF langT _     eStyle  = 
     vsep ["edge",
                               lbracket,
                               "  " <+> (align . vsep) [
                                    "source" <+> (pretty . tshow . fromSqlKey . entityKey) langF,
                                    "target" <+> (pretty . tshow . fromSqlKey . entityKey) langT,
                                    "graphics",
                                    lbracket,
                                    "  " <+> (align . vsep) [
                                         "style"  <+> pretty eStyle,
                                         "targetArrow \"standard\""
                                        ],
                                    rbracket
                                    ],
                               rbracket
                              ]

prettyLang :: Entity Language -> Doc AnsiStyle
prettyLang lang = vsep ["node",
                        lbracket,
                        "  " <+> (align . vsep) [
                             "id" <+> (pretty . tshow . fromSqlKey . entityKey) lang,
                             "label" <+> (dquotes . pretty . tshow . entityVal) lang],
                        rbracket
                       ]
