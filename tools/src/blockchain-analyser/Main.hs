{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}
module Main where

import           Mockable                  (runProduction)
import           Pos.Block.Core            (Block)
import           Pos.Block.Types           (Undo)
import           Pos.Core                  (HasConfiguration, headerHash)
import           Pos.Core.Types            (HeaderHash)
import           Pos.DB                    (closeNodeDBs, openNodeDBs)
import qualified Pos.DB.Block              as DB
import qualified Pos.DB.DB                 as DB
import           Pos.Launcher              (withConfigurations)
import           Pos.Ssc.GodTossing        (SscGodTossing)
import           Pos.Util.Chrono           (NewestFirst (..))
import           System.Directory          (canonicalizePath, doesDirectoryExist,
                                            getFileSize, listDirectory,
                                            withCurrentDirectory)

import           Options                   (CLIOptions (..), getOptions)
import           Rendering                 (render, renderBlock, renderBlocks,
                                            renderHeader)
import           System.Wlog               (LoggerName (..))
import           System.Wlog.LoggerNameBox (HasLoggerName (..))
import           Types                     (BlockchainInspector, DBFolderStat,
                                            initBlockchainAnalyser, prevBlock)

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

-- | Calculate the size of each folder.
dbSizes :: FilePath -> IO [DBFolderStat]
dbSizes root = do
    parents <- ls root
    forM (root : parents) $ \f -> (toText f,) <$> du_s f

-- | Analyse the blockchain, printing useful statistics.
analyseBlockchain :: HasConfiguration => CLIOptions -> HeaderHash -> BlockchainInspector ()
analyseBlockchain cli tip =
    if incremental cli then do putText (renderHeader cli)
                               analyseBlockchainEagerly cli tip
                       else analyseBlockchainLazily cli

-- | Tries to fetch a `Block` given its `HeaderHash`.
fetchBlock :: HasConfiguration => HeaderHash -> BlockchainInspector (Maybe (Block SscGodTossing))
fetchBlock = DB.blkGetBlock @SscGodTossing

-- | Tries to fetch an `Undo` for the given `Block`.
fetchUndo :: HasConfiguration => Block SscGodTossing -> BlockchainInspector (Maybe Undo)
fetchUndo block = DB.blkGetUndo @SscGodTossing (headerHash block)

-- | Analyse the blockchain lazily by rendering all the blocks at once, loading the whole
-- blockchain into memory. This mode generates very nice-looking tables, but using it for
-- big DBs might not be feasible.
analyseBlockchainLazily :: HasConfiguration => CLIOptions -> BlockchainInspector ()
analyseBlockchainLazily cli = do
    allBlocks <- map (bimap identity Just) . getNewestFirst <$> DB.loadBlundsFromTipWhile (const True)
    putText (renderBlocks cli allBlocks)

-- | Analyse the blockchain eagerly, rendering a block at time, without loading the whole
-- blockchain into memory.
analyseBlockchainEagerly :: HasConfiguration => CLIOptions -> HeaderHash -> BlockchainInspector ()
analyseBlockchainEagerly cli currentTip = do
    let processBlock block mbUndo = do putText (renderBlock cli (block, mbUndo))
                                       analyseBlockchainEagerly cli (prevBlock block)
    nextBlock <- fetchBlock currentTip
    case nextBlock of
        Nothing -> return ()
        Just b  -> fetchUndo b >>= processBlock b

-- Quick hack. Orphan is acceptableh here as this is an executable we'll never depend upon.
instance HasLoggerName IO where
    getLoggerName = return $ LoggerName "BlockchainAnalyser"
    modifyLoggerName _ = identity

-- | The main entrypoint.
main :: IO ()
main = do
    cli@CLIOptions{..} <- getOptions
    withConfigurations configOptions $ do
        -- Render the first report
        sizes <- canonicalizePath dbPath >>= dbSizes
        putText $ render uom printMode sizes

        -- Now open the DB and inspect it, generating the second report
        bracket (openNodeDBs False dbPath) closeNodeDBs $ \db -> do
            runProduction $ initBlockchainAnalyser db $ do
                DB.getTip >>= analyseBlockchain cli
