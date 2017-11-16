module Lang.DisplayError
    ( ppArgumentError
    , ppEvalError
    , ppTypeError
    , ppTypeName
    , ppProcError
    ) where

import           Universum hiding (empty, (<$>))

import qualified Data.Set as S

import           Data.Text (unpack)
import           Data.Text.Buildable (build)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder
import           Text.PrettyPrint.ANSI.Leijen (Doc, bold, char, comma, empty, hcat, indent,
                                               punctuate, squotes, text, vcat, yellow, (<$>), (<+>))

import           Lang.Argument (ArgumentError (..), ProcError (..), TypeError (..), TypeName (..),
                                isEmptyArgumentError)
import           Lang.Interpreter (EvalError (..))
import           Lang.Name (Name)


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
        else "Following type errors occured:" <$> (indent 2 . hcat . map ppTypeError . S.toList) peTypeErrors

ppEvalError :: EvalError -> Doc
ppEvalError (CommandNotSupported name) =
        "Command" <+> (squotes . highlight . nameToDoc) name
    <+> "is anavailable. Try" <+> (squotes . highlight) "help"
ppEvalError (InvalidArguments name procError) =
        "Invalid arguments for" <+> (squotes . highlight . nameToDoc) name <> ":"
    <$> indent 2 (ppProcError procError)
