#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Foldl   as F
import           Data.Function   (on)
import           Data.List       (foldl', sortBy)
import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import           Prelude         hiding (FilePath)
import           Turtle

main :: IO ()
main = do
    xs <- arguments
    case xs of
      [folder, file] -> do
          folder' <- realpath $ fromText folder
          let file' = fromText file
              md    = filesToMd $ hsFiles folder'
          case extension file' of
            Just "md"  -> output file' md
            Just "pdf" -> pandoc file md
            _          -> err "Expected *.md or *.pdf extension for output file."
      _              -> err "Expected source folder and output file as arguments."

type Tag = Text

type ModuleName = Maybe Text

type Body = [Either Tag Text]

data Check = Check
    { chTag  :: !(Maybe Tag)
    , chPos  :: !Int
    , chBody :: !Body
    } deriving Show

data Module = Module
    { modName   :: !ModuleName
    , modChecks :: ![Check]
    } deriving Show

hsFiles :: FilePath -> Shell FilePath
hsFiles folder = do
    file <- lstree folder
    case extension file of
        Just "hs" -> return file
        _         -> mzero

filesToMd :: Shell FilePath -> Shell Text
filesToMd files = do
    ms <- sortBy (compare `on` modName) <$> fold (files >>= fileToModule) F.list
    select $ renderModules ms

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

fileToModule :: FilePath -> Shell Module
fileToModule file = toModule <$> fold (input file) F.list

renderModules :: [Module] -> [Text]
renderModules xs =
    let checks = concat $ modChecks <$> xs
        m      = tagToBody checks
    in  "# Checks" : concatMap (renderModule m) xs

  where

    tagToBody :: [Check] -> M.Map Tag Body
    tagToBody = foldl' h M.empty where

        h :: M.Map Tag Body -> Check -> M.Map Tag Body
        h m c = case chTag c of
            Nothing -> m
            Just t  -> case M.lookup t m of
                         Nothing -> M.insert t (chBody c) m
                         Just _  -> error $ "duplicate tag " ++ show t

    renderModule :: M.Map Tag Body -> Module -> [Text]
    renderModule m md =
        case modChecks md of
          [] -> []
          cs -> "" : renderModuleName (modName md) : concatMap (renderCheck m) cs

    renderModuleName :: ModuleName -> Text
    renderModuleName Nothing  = "## Unknown Module"
    renderModuleName (Just n) = "## Module " <> n

    renderCheck :: M.Map Tag Body -> Check -> [Text]
    renderCheck m c = "" : renderBody m (chBody c) ++ renderPos (chPos c)

    renderBody :: M.Map Tag Body -> Body -> [Text]
    renderBody m bs = do
        b <- bs
        case b of
          Left t  -> case M.lookup t m of
                       Nothing -> error $ "unknown tag " ++ show t
                       Just b' -> renderBody m b'
          Right t -> return t

    renderPos :: Int -> [Text]
    renderPos p = ["", fromString $ "_(line " ++ show p ++ ")_"]

toModule :: [Text] -> Module
toModule xs = Module (getModuleName xs) (getChecks xs)

  where

    getModuleName :: [Text] -> ModuleName
    getModuleName []     = Nothing
    getModuleName (t:ts) = case match moduleNamePattern t of
        [y] -> Just y
        _   -> getModuleName ts

    moduleNamePattern :: Pattern Text
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

    loop' :: [Check] -> Int -> Int -> Maybe Tag -> Body -> [Text] -> [Check]
    loop' acc _ j mt bs []       = reverse (Check mt j (reverse bs) : acc)
    loop' acc i j mt bs (t : ts) = case match checkComment t of
        [c]
            | (not $ T.null c) && T.head c == '#' -> loop' acc                             (succ i) j mt (Left (T.tail c) : bs) ts
            | otherwise                           -> loop' acc                             (succ i) j mt (Right c : bs)         ts
        _                                         -> loop  (Check mt j (reverse bs) : acc) (succ i)                             ts

    noHyphens :: Pattern Text
    noHyphens = star $ notChar '-'

    checkPattern :: Pattern Text
    checkPattern = noHyphens *> (text "-- CHECK: " <|> text "-- | CHECK: ") *> chars

    checkComment :: Pattern Text
    checkComment = do
        ys <- noHyphens *> ((text "-- " *> chars) <|> (text "--" *> pure ""))
        return $ if T.length ys <= 1 || T.head ys /= '|'
           then ys
           else T.tail $ T.tail ys
