{-# LANGUAGE TemplateHaskell     #-}

#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import           Development.GitRev           (gitBranch, gitHash)
import           System.Wlog                  (logInfo, usingLoggerName)
import qualified Control.Foldl                as F
import           Data.Function                (on)
import           Data.List                    (foldl', sortBy)
import qualified Data.Map.Strict              as M
import           Data.String.QQ               (s)
import qualified Data.Text                    as T
import           Data.Version                 (showVersion)
import           Options.Applicative.Simple   (Parser, execParser, footerDoc, fullDesc,
                                               help, helper, info, infoOption, long,
                                               metavar, progDesc, short)
import qualified Options.Applicative.Simple   as S
import           Options.Applicative.Text     (textOption)
import           Paths_cardano_sl             (version)
import           Prelude                      hiding (FilePath)
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Turtle                       hiding (s)

data ChecksOptions = ChecksOptions
    { sourcesDir :: !Text
    , outputFile :: !Text
    } deriving (Show)

optionsParser :: Parser ChecksOptions
optionsParser = do
    sourcesDir <- textOption $
           short   's'
        <> long    "sources-dir"
        <> metavar "PATH"
        <> help    "Path to directory with Haskell source files."
    outputFile <- textOption $
           short   'o'
        <> long    "output"
        <> metavar "PATH"
        <> help    "Path to output file, *.md or *.pdf are assumed."
    pure ChecksOptions{..}

getChecksOptions :: IO ChecksOptions
getChecksOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> optionsParser) $
        fullDesc <> progDesc ("Extract comments from Haskell source code and store it in output file.")
                 <> S.header "Extractor of checks comments."
                 <> footerDoc usageExample

    versionOption = infoOption
        ("cardano-checks-" <> showVersion version)
        (long "version" <> help "Show version.")

usageExample :: Maybe Doc
usageExample = Just [s|
Command example:

  stack exec -- cardano-checks -s /tmp/cardano-sl -o /tmp/checks.md

Example of output file content:

  # Checks

  ## Module Pos.Crypto.SecretSharing
  Verify an encrypted share using SecretSharingExtra.
  _(line 182)_
  Verify that Share has been decrypted correctly.
  _(line 188)_
  Verify that SecretProof corresponds to Secret.
  _(line 194)_

  ## Module Pos.Crypto.Signing
  Verify a signature.|]

main :: IO ()
main = do
    usingLoggerName "kifla" $ logInfo $ "cardano-sl, commit " <> $(gitHash) <> " @ " <> $(gitBranch)
    ChecksOptions{..} <- getChecksOptions
    folder' <- realpath $ fromText sourcesDir
    let file' = fromText outputFile
        md    = filesToMd $ hsFiles folder'
    case extension file' of
        Just "md"  -> output file' md
        Just "pdf" -> pandoc outputFile md
        _          -> err "Expected *.md or *.pdf extension for output file."

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

filesToMd :: Shell FilePath -> Shell Line
filesToMd files = do
    ms <- sortBy (compare `on` modName) <$> fold (files >>= fileToModule) F.list
    select . textToLines =<< select (renderModules ms)

pandoc :: Text -> Shell Line -> IO ()
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
fileToModule file = toModule . map lineToText <$> fold (input file) F.list

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
