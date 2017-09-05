{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}
module Main where

import           Formatting
import           Mockable           (runProduction)
import           Pos.Block.Core     (GenesisBlock, MainBlock)
import           Pos.Core           (HasCoreConstants, gbHeader, gbPrevBlock,
                                     gbhPrevBlock, giveStaticConsts, headerHash)
import           Pos.Core.Block     (GenericBlock (..))
import           Pos.Core.Types     (HeaderHash)
import           Pos.DB             (closeNodeDBs, openNodeDBs)
import           Pos.DB             (MonadBlockDBGeneric (..))
import qualified Pos.DB.Block       as DB
import qualified Pos.DB.DB          as DB
import           Pos.Ssc.GodTossing (SscGodTossing)
import           Pos.Util.Chrono    (OldestFirst (..))
import           System.Directory   (canonicalizePath, doesDirectoryExist, getFileSize,
                                     listDirectory, withCurrentDirectory)

import           Options            (CLIOptions (..), PrintMode, getOptions)
import           Rendering          (render, renderBlock)
import           Types              (BlockchainInspector, DBFolderStat,
                                     initBlockchainAnalyser)

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
main = giveStaticConsts $ do
    CLIOptions{..} <- getOptions
    sizes <- canonicalizePath dbPath >>= dbSizes
    putText $ render uom printMode sizes

    -- Now open the DB and inspect it
    bracket (openNodeDBs False dbPath) closeNodeDBs $ \db -> do
        runProduction $ initBlockchainAnalyser db $ do
            tip <- DB.getTip
            analyseBlockchain printMode tip

analyseBlockchain :: HasCoreConstants => PrintMode -> HeaderHash -> BlockchainInspector ()
analyseBlockchain pMode currentTip = do
    mbBlock <- DB.dbGetBlockSumDefault @SscGodTossing currentTip
    case mbBlock of
        Nothing -> return ()
        Just block@(Right mB) -> do
            liftIO $ putText (renderBlock pMode block)
            analyseBlockchain pMode (mB ^. gbPrevBlock)
        Just block@(Left  gB) -> do
            liftIO $ putText (renderBlock pMode block)
            analyseBlockchain pMode (gB ^. gbHeader . gbhPrevBlock)
