module Lang.DisplayError
    ( ppArgumentError
    , ppEvalError
    , ppTypeError
    , ppTypeName
    , ppParseError
    , ppProcError
    ) where

import           Universum hiding (empty, (<$>))

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text.Lazy as T


import           Data.Loc (Span, loc, locColumn, locLine, spanEnd, spanFromTo, spanStart, toNat)
import           Data.Loc.Span (joinAsc)
import           Data.Text (unpack)
import           Data.Text.Buildable (build)
import           Data.Text.Lazy.Builder (toLazyText)
import           Text.Earley (Report (..))
import           Text.PrettyPrint.ANSI.Leijen (Doc, bold, char, comma, empty, hcat, indent,
                                               punctuate, squotes, text, vcat, yellow, (<$>), (<+>))

import           Lang.Argument (ArgumentError (..), ProcError (..), TypeError (..), TypeName (..),
                                isEmptyArgumentError)
import           Lang.Interpreter (EvalError (..))
import           Lang.Lexer (Token (..))
import           Lang.Name (Name)
import           Lang.Parser (ParseError (..))


highlight :: Doc -> Doc
highlight = bold . yellow

nameToDoc :: Name -> Doc
nameToDoc = text . T.unpack . toLazyText . build

ppTypeName :: TypeName -> Doc
ppTypeName (TypeName name)         = text (unpack name)
ppTypeName (TypeNameEither tn tn') = ppTypeName tn <+> char '|' <+> ppTypeName tn'

ppTypeError :: TypeError -> Doc
ppTypeError TypeError{..} =
        "Couldn't match expected type" <+> (bold . yellow $ ppTypeName teExpectedType)
    <+> "with actual value"            <+> (bold . yellow $ show teActualValue)
    <>  "!"

ppArgumentError :: ArgumentError -> Doc
ppArgumentError ae@ArgumentError{..} =
    if isEmptyArgumentError ae
    then empty
    else vcat $ missingKeysDoc <> irrelevantKeysDoc <> irrelevantPosDoc
  where
    setToDoc = hcat . punctuate comma . map (highlight . nameToDoc) . S.toList
    missingKeysDoc =
        if S.null aeMissingKeys
        then []
        else ["Missing keys:" <+> setToDoc aeMissingKeys]
    irrelevantKeysDoc =
        if S.null aeIrrelevantKeys
        then []
        else ["Irrelevant keys:" <+> setToDoc aeIrrelevantKeys]
    irrelevantPosDoc =
        if aeIrrelevantPos == 0
        then []
        else ["Irrelevant positional argument:" <+> (highlight . show) aeIrrelevantPos]

ppProcError :: ProcError -> Doc
ppProcError ProcError{..} = ppArgumentError peArgumentError <$> typeErrorsDoc
  where
    typeErrorsDoc =
        if S.null peTypeErrors
        then empty
        else "Following type errors occured:" <$>
             (indent 2 . hcat . map ppTypeError . S.toList) peTypeErrors

ppEvalError :: EvalError -> Doc
ppEvalError (CommandNotSupported name) =
        "Command" <+> (squotes . highlight . nameToDoc) name
    <+> "is unavailable. Try" <+> (squotes . highlight) "help"
ppEvalError (InvalidArguments name procError) =
        "Invalid arguments for" <+> (squotes . highlight . nameToDoc) name <> ":"
    <$> indent 2 (ppProcError procError)


renderLine :: Int -> Int -> Text -> Doc
renderLine start end str = (text $ unpack str)
  <$> highlight (indent start . hcat . replicate (end - start) $ char '^')

renderFullLine :: Text -> Doc
renderFullLine str = renderLine 0 (length str) str

ppParseError :: ParseError -> Doc
ppParseError (ParseError str (Report {..})) =
      "Parse error at" <+> text (show span)
  <$> "Unexpected" <+> text unconsumedDesc <> ", expected"
  <+> hcat (punctuate (text ", or ") $ map (text . unpack) expected)
  <$> renderLines
  where
    unconsumedDesc = maybe "end of input" show . head . fmap snd $ unconsumed
    strLines = NE.nonEmpty $ take spanLines . drop (spanLineStart - 1) $ lines str
    renderLines = case strLines of
        Nothing -> error "Wtf"
        Just (line :| []) -> renderLine (spanColumnStart - 1) (spanColumnEnd - 1) line
        Just (line :| (line' : ls')) ->  let ls = line' :| ls' in
                renderLine (spanColumnStart - 1) (length line) line
            <$> (vcat . map renderFullLine . NE.init $ ls)
            <$> renderLine 0 (spanColumnEnd - 1) (NE.last ls)

    spanColumnStart = fromIntegral . toNat . locColumn . spanStart $ span
    spanColumnEnd   = fromIntegral . toNat . locColumn . spanEnd   $ span
    spanLineStart   = fromIntegral . toNat . locLine   . spanStart $ span
    spanLineEnd     = fromIntegral . toNat . locLine   . spanEnd   $ span
    spanLines = spanLineEnd - spanLineStart + 1

    strEndLoc = loc 1 (fromInteger . toInteger . length $ str)

    addColumn col l = loc (locLine l) (locColumn l + col)
    isTokenUnknown (TokenUnknown _) = True
    isTokenUnknown _                = False
    unknownSpans :: [Span]
    unknownSpans = map fst . takeWhile (isTokenUnknown . snd) $ unconsumed
    span = NE.head $
        case NE.nonEmpty (joinAsc unknownSpans) <|> NE.nonEmpty (map fst unconsumed) of
            Nothing -> spanFromTo strEndLoc (addColumn 1 strEndLoc) :|[]
            Just x  -> x
