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
     migrated <- listMigratedWordsByLangFromAndTo (getLangName langFrom) (getLangName langTo)
     derivated <- listDerivatedWordsByLangFromAndTo (getLangName langFrom) (getLangName langTo)
     let prettyEdgeLL ws = prettyEdge langFrom langTo ((not . null) ws)
     return $ vsep [
          prettyEdgeLL evolved "\"standard\"" "2",
          prettyEdgeLL migrated "\"standard\"" "1",
          prettyEdgeLL combined "\"dashed\"" "1",
          prettyEdgeLL derivated "\"dotted\"" "1"
          ]
     where
          getLangName = languageLname . entityVal


prettyEdge :: Entity Language -> Entity Language -> Bool -> Text -> Text -> Doc AnsiStyle
prettyEdge _     _     False _      _       = mempty
prettyEdge langF langT _     eStyle eWidth  =
     vsep ["edge",
                               lbracket,
                               "  " <+> (align . vsep) [
                                    "source" <+> (pretty . tshow . fromSqlKey . entityKey) langF,
                                    "target" <+> (pretty . tshow . fromSqlKey . entityKey) langT,
                                    "graphics",
                                    lbracket,
                                    "  " <+> (align . vsep) [
                                         "style"  <+> pretty eStyle,
                                         "targetArrow \"standard\"",
                                         "width" <+> pretty eWidth
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
                             "label" <+> (dquotes . pretty . tshow . entityVal) lang,
                             "graphics",
                             lbracket,
                             "  " <+> (align . vsep) [
                                  "type" <+> (dquotes . pretty) ("rectangle" :: Text),
                                  "fill" <+> (dquotes . pretty) ("#FFCC00" :: Text)],
                             rbracket,
                             "LabelGraphics",
                             lbracket,
                             "  " <+> (align . vsep) [
                                   "text" <+> (dquotes . pretty . tshow . entityVal) lang,
                                   "fontSize 14"
                                   ],
                             rbracket],
                         rbracket]
