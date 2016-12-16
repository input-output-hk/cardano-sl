#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Foldl as F
import qualified Data.Map      as M
import qualified Data.Text     as T
import           Turtle
import           Prelude       hiding (FilePath)

main = putStrLn "to be implemented!"

{-
main :: IO ()
main = do
    xs <- arguments
    case xs of
        [folder, file] -> pandoc file $ folderMarkDown folder
        _              -> err "Expected source folder and output file as arguments."
-}

type Tag = Text

type ModuleName = Text

type ErrorMessage = Text

data Check = Check
    { chTag  :: !(Maybe Tag)
    , chPos  :: !Int
    , chBody :: ![Either Tag Text]
    } deriving Show

data Module = Module
    { modName   :: !ModuleName
    , modChecks :: ![Check]
    } deriving Show

toModule :: [Text] -> Either ErrorMessage Module
toModule xs = Module <$> getModuleName xs <*> pure (getChecks xs)

  where

    getModuleName :: [Text] -> Either ErrorMessage ModuleName
    getModuleName []     = Left "no module name"
    getModuleName (t:ts) = case match moduleNamePattern t of
        [y] -> Right y
        _   -> getModuleName ts

    moduleNamePattern :: Pattern ModuleName
    moduleNamePattern = text "module " *> star (noneOf " ") <* (char ' ' <* chars <|> eof *> pure ' ')

    getChecks :: [Text] -> [Check]
    getChecks = loop [] 1

    loop :: [Check] -> Int -> [Text] -> [Check]
    loop acc _ []     = reverse acc
    loop acc i (t:ts) = case match checkPattern t of
        [c]
            | T.null c        -> loop  acc (succ i)                                       ts
            | T.head c == '@' -> loop' acc (succ i) i (Just $ T.tail c) []                ts
            | T.head c == '#' -> loop' acc (succ i) i Nothing           [Left $ T.tail c] ts
            | otherwise       -> loop' acc (succ i) i Nothing           [Right c]         ts
        _                     -> loop  acc (succ i)                                       ts

    loop' :: [Check] -> Int -> Int -> Maybe Tag -> [Either Tag Text] -> [Text] -> [Check]
    loop' acc _ j mt bs []       = reverse (Check mt j (reverse bs) : acc)
    loop' acc i j mt bs (t : ts) = case match checkComment t of
        [c]
            | T.null c        -> loop  (Check mt j (reverse bs) : acc) (succ i)                             ts
            | T.head c == '#' -> loop' acc                             (succ i) j mt (Left (T.tail c) : bs) ts
            | otherwise       -> loop' acc                             (succ i) j mt (Right c : bs)         ts
        _                     -> loop  (Check mt j (reverse bs) : acc) (succ i)                             ts

    checkPattern :: Pattern Text
    checkPattern = chars *> text "-- CHECK: " *> chars

    checkComment :: Pattern Text
    checkComment = chars *> text "-- " *> chars

{-
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
            (Module               <$> (text "module " *> star (noneOf " ") <* rest))
        <|> (CheckHeader (succ n) <$> (chars *> text "-- CHECK # " *> chars))
        <|> (CheckItem   (succ n) <$> (chars *> text "-- CHECK ## " *> chars))

    rest = (char ' ' <* chars <|> eof *> pure ' ')

    fromCheck :: Check -> Shell Text
    fromCheck (Module t)        = return "" <|> return ("## " <> t)
    fromCheck (CheckHeader n t) = return "" <|> (return $ "### " <> t <> line n)
    fromCheck (CheckItem   n t) = return ("  * " <> t <> line n)

    line :: Int -> Text
    line n = " _(line " <> fromString (show n) <> ")_"
    -}
