#!/usr/bin/env stack
-- stack --install-ghc runghc

{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative          ((<|>))
import           Control.Monad                (forM)
import           Control.Monad.IO.Class       (MonadIO (..))
import qualified Data.Attoparsec.Text         as P
import           Data.Function                (on)
import           Data.List                    (foldl', sortBy)
import qualified Data.Map.Strict              as M
import           Data.Monoid                  ((<>))
import           Data.String.QQ               (s)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Data.Version                 (showVersion)
import           Options.Applicative.Simple   (Parser, execParser, footerDoc, fullDesc,
                                               help, helper, info, infoOption, long,
                                               metavar, progDesc, short)
import qualified Options.Applicative.Simple   as S
import           Options.Applicative.Text     (textOption)
import           Paths_cardano_sl             (version)
import           Pos.Util                     (lstree)
import           Prelude
import           System.Directory             (canonicalizePath)
import           System.FilePath              (takeExtension)
import qualified System.IO                    as IO
import           System.Process               (readProcess)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

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
    ChecksOptions{..} <- getChecksOptions
    folder' <- canonicalizePath (T.unpack sourcesDir)
    let file' = T.unpack outputFile
    md    <- filesToMd =<< hsFiles folder'
    case takeExtension file' of
        ".md"  -> do
            T.writeFile file' mempty
            mapM_ (T.appendFile file') md
        ".pdf" -> pandoc file' md
        _      -> err "Expected *.md or *.pdf extension for output file."

-- | Print exactly one line to @stderr@
err :: MonadIO m => T.Text -> m ()
err = liftIO . T.hPutStrLn IO.stderr

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


hsFiles :: MonadIO m => FilePath -> m [FilePath]
hsFiles folder = filter ((== ".hs") . takeExtension) <$> (lstree folder)

filesToMd :: MonadIO m => [FilePath] -> m [T.Text]
filesToMd files = do
    ms <- sortBy (compare `on` modName) <$> (forM files fileToModule)
    return (renderModules ms)

pandoc :: FilePath -> [T.Text] -> IO ()
pandoc file markdown = do
    _ <- readProcess "pandoc" [ "-f"
                              , "markdown"
                              , "-t"
                              , "latex"
                              , "-o"
                              , file
                              ]
         (T.unpack . T.unlines $ markdown)
    return ()

fileToModule :: MonadIO m => FilePath -> m Module
fileToModule file = toModule . T.lines <$> liftIO (T.readFile file)

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
    renderPos p = ["", T.pack $ "_(line " ++ show p ++ ")_"]

toModule :: [Text] -> Module
toModule xs = Module (getModuleName xs) (getChecks xs)

  where

    chars :: P.Parser Text
    chars = star P.anyChar

    noneOf :: [Char] -> P.Parser Char
    noneOf cs = P.satisfy (`notElem` cs)

    star :: P.Parser Char -> P.Parser Text
    star = fmap T.pack . P.many'

    getModuleName :: [Text] -> ModuleName
    getModuleName []     = Nothing
    getModuleName (t:ts) = case P.parseOnly (P.many' moduleNamePattern) t of
        Right [y] -> Just y
        _         -> getModuleName ts

    moduleNamePattern :: P.Parser Text
    moduleNamePattern = P.string "module " *> star (noneOf " ") <* (P.char ' ' <* chars <|> P.endOfInput *> pure ' ')

    getChecks :: [Text] -> [Check]
    getChecks = loop [] 1

    loop :: [Check] -> Int -> [Text] -> [Check]
    loop acc _ []     = reverse acc
    loop acc i (t:ts) = case P.parseOnly (P.many' checkPattern) t of
        Right [c]
            | T.null c        -> loop  acc (succ i)                                       ts
            | T.head c == '@' -> loop' acc (succ i) i (Just $ T.tail c) []                ts
            | T.head c == '#' -> loop' acc (succ i) i Nothing           [Left $ T.tail c] ts
            | otherwise       -> loop' acc (succ i) i Nothing           [Right c]         ts
        _                     -> loop  acc (succ i)                                       ts

    loop' :: [Check] -> Int -> Int -> Maybe Tag -> Body -> [Text] -> [Check]
    loop' acc _ j mt bs []       = reverse (Check mt j (reverse bs) : acc)
    loop' acc i j mt bs (t : ts) = case P.parseOnly (P.many' checkComment) t of
        Right [c]
            | (not $ T.null c) && T.head c == '#' -> loop' acc                             (succ i) j mt (Left (T.tail c) : bs) ts
            | otherwise                           -> loop' acc                             (succ i) j mt (Right c : bs)         ts
        _                                         -> loop  (Check mt j (reverse bs) : acc) (succ i)                             ts

    noHyphens :: P.Parser Text
    noHyphens = star $ P.notChar '-'

    checkPattern :: P.Parser Text
    checkPattern = noHyphens *> (P.string "-- CHECK: " <|> P.string "-- | CHECK: ") *> chars

    checkComment :: P.Parser Text
    checkComment = do
        ys <- noHyphens *> ((P.string "-- " *> chars) <|> (P.string "--" *> pure ""))
        return $ if T.length ys <= 1 || T.head ys /= '|'
           then ys
           else T.tail $ T.tail ys
