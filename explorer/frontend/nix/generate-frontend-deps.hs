#! /usr/bin/env nix-shell
#! nix-shell shell.nix -i runhaskell
{-# LANGUAGE OverloadedStrings, LambdaCase, NoImplicitPrelude #-}

import Universum hiding (FilePath, die, fold, (<>))
import qualified Data.Text as T
import Turtle
import System.IO (withFile, IOMode(WriteMode))
import qualified Control.Foldl as Fold
import Data.Foldable (find)

main :: IO ()
main = do
  testMode <- options "Regenerate frontend dependencies" optionsParser
  sh $ if testMode
    then test
    else regen

regen :: Shell ()
regen = do
  echo "Regenerating nix for frontend"
  bower2nix "bower.json" "nix/bower-generated.nix"

bower2nix :: FilePath -> FilePath -> Shell ()
bower2nix src out = cachedShell [src] out $ \out' ->
  procs "bower2nix" [tt src, tt out'] empty

test :: Shell ()
test = do
  echo "Checking that auto-generated frontend dependencies nix is up to date."
  whenM (needsChange ["bower.json"] "nix/bower-generated.nix") $ do
    echo " - bower-generated.nix needs update"
    die "Consult explorer/frontend/README.md to fix this"

optionsParser :: Parser Bool
optionsParser = switch "test" 't' "Test freshness but don't regenerate"

----------------------------------------------------------------------------

-- | Run a shell command only if the destination file is out of date
-- with respect to the source file.
cachedShell :: [FilePath] -> FilePath -> (FilePath -> Shell ()) -> Shell ()
cachedShell srcs dst action = needsChange srcs dst >>= \case
  True -> do
    printf ("Generating " % fp % "\n") dst
    tmp <- mktempfile "." (tt $ basename dst)
    action tmp
    whenM (testpath dst) $ rm dst
    input tmp & stampFile srcs & output dst
  False -> printf (fp % " is already up to date according to " % s % "\n")
               dst (T.intercalate ", " $ map tt srcs)

-- | A file needs a change if its hash doesn't match or it doesn't exist.
needsChange :: [FilePath] -> FilePath -> Shell Bool
needsChange srcs dst = do
  exists <- testfile dst
  if exists
    then do
      let stamps = flip fold Fold.list . limit (length srcs)
      line <- stamps $ input dst
      hash <- stamps $ hashFile srcs
      pure $ or [ l /= h | (l, h) <- zip line hash ]
    else pure True

-- | sha256sum output prepended with a nix line comment symbol
hashFile :: [FilePath] -> Shell Line
hashFile srcs = fmap ("# " <>) (inproc "sha256sum" (map tt srcs) empty)

-- | Adds a hash to the top of the file
stampFile :: [FilePath] -> Shell Line -> Shell Line
stampFile ref f = cat [hashFile ref, f]

tt = format fp
