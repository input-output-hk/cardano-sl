module Lang.DisplayError
    ( ppArgumentError
    , ppEvalError
    , ppTypeError
    , ppTypeName
    , ppParseError
    , ppProcError
    , renderAuxxDoc
    ) where

import           Universum hiding (empty, (<$>))

import qualified Data.List.NonEmpty as NE
import qualified Text.PrettyPrint.ANSI.Leijen

import           Control.Lens (matching)
import           Data.Loc (Span, loc, locColumn, locLine, spanEnd, spanFromTo, spanStart, toNat)
import           Data.Loc.Span (joinAsc)
import           Data.Text.Buildable (build)
import           Data.Text.Lazy.Builder (toLazyText)
import           Text.Earley (Report (..))
import           Text.PrettyPrint.ANSI.Leijen (Doc, bold, char, comma, empty, hcat, indent,
                                               punctuate, squotes, vcat, yellow, red, (<$>), (<+>))

import           Lang.Argument (ArgumentError (..), ProcError (..), TypeError (..), TypeName (..),
                                isEmptyArgumentError)
import           Lang.Interpreter (EvalError (..))
import           Lang.Lexer (_TokenUnknown)
import           Lang.Name (Name)
import           Lang.Parser (ParseError (..))

{-# ANN module ("HLint: ignore Functor law" :: Text) #-}

highlight :: Doc -> Doc
highlight = bold . yellow

text :: Text -> Doc
text = Text.PrettyPrint.ANSI.Leijen.text . toString

nameToDoc :: Name -> Doc
nameToDoc = Text.PrettyPrint.ANSI.Leijen.text . toString . toLazyText . build

ppTypeName :: TypeName -> Doc
ppTypeName (TypeName name)         = text name
ppTypeName (TypeNameEither tn tn') = ppTypeName tn <+> char '|' <+> ppTypeName tn'

ppTypeError :: TypeError -> Doc
ppTypeError TypeError{..} =
        "Couldn't match expected type" <+> (highlight $ ppTypeName teExpectedType)
    <+> "with actual value"            <+> (highlight $ show teActualValue) -- TODO: CSL-1814
    <>  "!"

ppArgumentError :: ArgumentError -> Doc
ppArgumentError ae@ArgumentError{..} =
    if isEmptyArgumentError ae
    then empty
    else vcat $ missingKeysDoc <> irrelevantKeysDoc <> irrelevantPosDoc
  where
    setToDoc = hcat . punctuate comma . map (highlight . nameToDoc) . toList
    missingKeysDoc =
        if null aeMissingKeys
        then []
        else ["Missing keys:" <+> setToDoc aeMissingKeys]
    irrelevantKeysDoc =
        if null aeIrrelevantKeys
        then []
        else ["Irrelevant keys:" <+> setToDoc aeIrrelevantKeys]
    irrelevantPosDoc =
        if aeIrrelevantPos == 0
        then []
        else ["Irrelevant positional arguments:" <+> (highlight . show) aeIrrelevantPos]

ppProcError :: ProcError -> Doc
ppProcError ProcError{..} = ppArgumentError peArgumentError <$> typeErrorsDoc
  where
    typeErrorsDoc =
        if null peTypeErrors
        then empty
        else "Following type errors occured:" <$>
             (indent 2 . hcat . map ppTypeError . toList) peTypeErrors

ppEvalError :: EvalError -> Doc
ppEvalError (CommandNotSupported name) =
        "Command" <+> (squotes . highlight . nameToDoc) name
    <+> "is unavailable. Try" <+> (squotes . highlight) "help"
ppEvalError (InvalidArguments name procError) =
        "Invalid arguments for" <+> (squotes . highlight . nameToDoc) name <> ":"
    <$> indent 2 (ppProcError procError)


renderLine :: Int -> Int -> Text -> Doc
renderLine start end str = text str
  <$> (bold . red . indent start . hcat . replicate (end - start) $ char '^')

renderFullLine :: Text -> Doc
renderFullLine str = renderLine 0 (length str) str

ppParseError :: ParseError -> Doc
ppParseError (ParseError str (Report {..})) =
      "Parse error at" <+> text (show span)
  <$> "Unexpected" <+> text unconsumedDesc <> ", expected"
  <+> hcat (punctuate (text ", or ") $ map text expected)
  <$> renderLines
  where
    unconsumedDesc = maybe "end of input" show . head . fmap snd $ unconsumed
    strLines = nonEmpty $ take spanLines . drop (spanLineStart - 1) $ lines str
    renderLines = case strLines of
        Nothing ->
            -- This can only happen if megaparsec's 'getPosition' somehow
            -- returned line number bigger than the actual amount of
            -- lines in the input text.
            error "ppParseError/renderLines: span is outside of the input bounds"
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

    isTokenUnknown = isRight . matching _TokenUnknown
    unknownSpans :: [Span]
    unknownSpans = map fst . takeWhile (isTokenUnknown . snd) $ unconsumed
    span = NE.head $
        case nonEmpty (joinAsc unknownSpans) <|> nonEmpty (map fst unconsumed) of
            Nothing -> spanFromTo strEndLoc (addColumn 1 strEndLoc) :|[]
            Just x  -> x

renderAuxxDoc :: Doc -> Text
renderAuxxDoc = show -- it's fine
