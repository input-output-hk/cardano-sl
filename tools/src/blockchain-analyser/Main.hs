{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}
module Main where

import           Options          (CLIOptions (..), getOptions, renderBytes)
import           System.Directory (canonicalizePath, doesDirectoryExist, getFileSize,
                                   listDirectory, withCurrentDirectory)
import           Universum

import           Text.Tabl        (Alignment (..), Decoration (..),
                                   Environment (EnvAscii), tabl)

-- | Like Unix's `du -s`, but works across all the major platforms and
-- returns the total number of bytes the directory occupies on disk.
-- Recurses inside each directory it finds.
du_s :: FilePath -> IO Integer
du_s root = go 0 [root]
  where
    go !acc [] = return acc
    go !acc (f:fs) = do
        sz        <- getFileSize f
        isDir     <- doesDirectoryExist f
        extraDirs <- if isDir then ls f else return mempty
        go (acc + sz) (extraDirs <> fs)

ls :: FilePath -> IO [FilePath]
ls f = withCurrentDirectory f ((listDirectory >=> mapM canonicalizePath) f)

dbSizes :: FilePath -> IO [(Text, Integer)]
dbSizes root = do
    parents <- ls root
    forM (root : parents) $ \f -> (toText f,) <$> du_s f

main :: IO ()
main = do
    CLIOptions{..} <- getOptions
    sizes <- canonicalizePath dbPath >>= dbSizes
    let rows = ["Directory", "Size"] : map (\(f,sz) -> [f, renderBytes uom sz]) sizes
    putText $ tabl EnvAscii hdecor vdecor aligns rows
  where
    hdecor = DecorUnion [DecorOuter, DecorOnly [1]]
    vdecor = DecorAll
    aligns = [AlignLeft, AlignLeft]
