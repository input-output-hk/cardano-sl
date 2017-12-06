{-# LANGUAGE GADTs #-}

module Command.Help
       ( mkHelpMessage
       ) where

import           Universum

import qualified Data.Text as T

import           Pos.Util.Justify (leftAlign)

import           Lang.Argument (ArgCardinality (..), SomeArgCardinality (..), TypeName (..),
                                getParameters)
import           Lang.Command (CommandProc (..), UnavailableCommand (..))
import           Lang.Name (Name)

commandHelp :: CommandProc m -> Text
commandHelp CommandProc{..} =
    let
        parameters = getParameters cpArgumentConsumer
        name = pretty cpName
        prefixes = name : repeat (T.replicate (T.length name) " ")
        helpLines = map (\l -> "-- " <> l <> "\n") $ leftAlign 40 cpHelp
        parameterLines =
            if null parameters
            then [""]
            else map parameterHelp parameters
        commandDesc = T.intercalate "\n" $
            zipWith (\p h -> p <> " " <> h) prefixes parameterLines
    in
        T.concat helpLines <> commandDesc

unavailableCommandHelp :: UnavailableCommand -> Text
unavailableCommandHelp UnavailableCommand{..} =
    pretty ucName <> " is unavailable because " <> ucReason

parameterHelp :: (Name, TypeName, SomeArgCardinality) -> Text
parameterHelp (name, tn, ac) = pretty name <> ": " <> withArgCardinality ac (withTypeName tn NeedWrap)

data NeedWrap = NeedWrap | DontNeedWrap

withArgCardinality :: SomeArgCardinality -> Text -> Text
withArgCardinality (SomeArgCardinality ac) = case ac of
    ArgCardSingle -> identity
    ArgCardOpt    -> (<> "?")
    ArgCardMany   -> (<> "*")
    ArgCardSome   -> (<> "+")

withTypeName :: TypeName -> NeedWrap -> Text
withTypeName (TypeName t) _ = t
withTypeName (TypeNameEither tn1 tn2) needWrap =
    case needWrap of
        DontNeedWrap -> t'
        NeedWrap     -> wrap t'
  where
    wrap t = "(" <> t <> ")"
    t' = withTypeName tn1 DontNeedWrap <> " | " <>
         withTypeName tn2 DontNeedWrap

mkHelpMessage :: [Either UnavailableCommand (CommandProc m)] -> Text
mkHelpMessage cps =
    "Available commands:\n\n" <>
    mconcat (map (\cp -> either unavailableCommandHelp commandHelp cp <> "\n\n") cps)
