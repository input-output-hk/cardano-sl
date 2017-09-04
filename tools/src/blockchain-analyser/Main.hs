{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}
module Main where

import           Mockable             (runProduction)
import           Pos.Block.Types      (Blund)
import           Pos.Core.Types       (HeaderHash)
import           Pos.DB               (closeNodeDBs, openNodeDBs)
import qualified Pos.DB.Block         as DB
import qualified Pos.DB.DB            as DB
import           Pos.DB.GState.Common (getTip)
import           Pos.Ssc.GodTossing   (SscGodTossing)
import           Pos.Util.Chrono      (OldestFirst (..))
import           System.Directory     (canonicalizePath, doesDirectoryExist, getFileSize,
                                       listDirectory, withCurrentDirectory)

import           Options              (CLIOptions (..), getOptions)
import           Rendering            (render)
import           Types                (DBFolderStat, initBlockchainAnalyser)

import           Universum

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

dbSizes :: FilePath -> IO [DBFolderStat]
dbSizes root = do
    parents <- ls root
    forM (root : parents) $ \f -> (toText f,) <$> du_s f

main :: IO ()
main = do
    CLIOptions{..} <- getOptions
    sizes <- canonicalizePath dbPath >>= dbSizes
    putText $ render uom printMode sizes

    -- Now open the DB

    bracket (openNodeDBs False dbPath) closeNodeDBs $ \db -> do
        res <- runProduction $ do
            initBlockchainAnalyser db $ getTip
        putText (toText ((show res) :: String))

{-
        initTBlockGenMode db genCtx $
            void $ evalRandT (genBlocks bgenParams) (mkStdGen seed)
-}

-- (OldestFirst [] (Blund SscGodTossing))
