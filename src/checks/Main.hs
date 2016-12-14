#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle
 
{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Foldl as Fold
import           Turtle
import           Prelude       hiding (FilePath)

main :: IO ()
main = do
    xs <- arguments
    case xs of
        [folder, file] -> pandoc file $ folderMarkDown folder
        _              -> err "Expected source folder and output file as arguments." 

data Check = Module !Text | CheckHeader !Text | CheckItem !Int !Text

hsFiles :: FilePath -> Shell FilePath
hsFiles folder = do
    file <- lstree folder
    case extension file of
        Just "hs" -> return file
        _         -> mzero


folderMarkDown :: Text -> Shell Text
folderMarkDown folder' = do
    folder <- realpath $ fromText folder'
    (return "# Checks") <|> (hsFiles folder >>= handleFile)

pandoc :: Text -> Shell Text -> IO ()
pandoc file markdown = procs
    "pandoc"
    [ "-f"
    , "markdown"
    , "-t"
    , "latex"
    , "-o"
    , file
    ]
    markdown

handleFile :: FilePath -> Shell Text
handleFile file = do
    let checks = do
            (n, l) <- nl $ input file
            case match (checkPattern n) l of
                [c] -> return c
                _   -> mzero
    l <- fold checks Fold.length
    guard (l > 1)
    checks >>= fromCheck

  where

    checkPattern :: Int -> Pattern Check
    checkPattern n =
            (Module             <$> (text "module " *> star (noneOf " ") <* rest))
        <|> (CheckHeader        <$> (chars *> text "-- CHECK # " *> chars))
        <|> (CheckItem (succ n) <$> (chars *> text "-- CHECK ## " *> chars))

    rest = (char ' ' <* chars <|> eof *> pure ' ')

    fromCheck :: Check -> Shell Text
    fromCheck (Module t)      = return "" <|> return ("## " <> t)
    fromCheck (CheckHeader t) = return "" <|> (return $ "### " <> t)
    fromCheck (CheckItem n t) = return ("  * " <> t) <|>
                                return (fromString $ "     + line " <> show n)

{-
handleFolder :: FilePath -> IO ()
handleFolder folder = stdout $ return "# Checks" <|> (hsFiles folder >>= handleFile)
-}
