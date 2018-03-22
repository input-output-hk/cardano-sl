{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}
module Main where

import           Universum

import           Mockable (Production, runProduction)
import           System.Directory (canonicalizePath, doesDirectoryExist, getFileSize, listDirectory,
                                   withCurrentDirectory)

import           Pos.Block.Types (Undo)
import qualified Pos.Client.CLI as CLI
import           Pos.Core (HasConfiguration, HeaderHash, headerHash)
import           Pos.Core.Block (Block)
import           Pos.DB (closeNodeDBs, openNodeDBs)
import           Pos.DB.Block (getUndo)
import qualified Pos.DB.Block.Load as DB
import           Pos.DB.Class (getBlock)
import qualified Pos.DB.GState.Common as GS
import           Pos.Launcher (withConfigurations)
import           Pos.Util.Chrono (NewestFirst (..))

import           Options (CLIOptions (..), getOptions)
import           Rendering (render, renderBlock, renderBlocks, renderHeader)
import           Types (BlockchainInspector, DBFolderStat, initBlockchainAnalyser, prevBlock)

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
fetchBlock :: HasConfiguration => HeaderHash -> BlockchainInspector (Maybe Block)
fetchBlock = getBlock

-- | Tries to fetch an `Undo` for the given `Block`.
fetchUndo :: HasConfiguration => Block -> BlockchainInspector (Maybe Undo)
fetchUndo = getUndo . headerHash

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

-- | The main entrypoint.
main :: IO ()
main = do
    args <- getOptions
    runProduction $ do
        CLI.printFlags
        action args

action :: CLIOptions -> Production ()
action cli@CLIOptions{..} = withConfigurations conf $ \_ -> do
    -- Render the first report
    sizes <- liftIO (canonicalizePath dbPath >>= dbSizes)
    liftIO $ putText $ render uom printMode sizes

    -- Now open the DB and inspect it, generating the second report
    bracket (openNodeDBs False dbPath) closeNodeDBs $ \db ->
        initBlockchainAnalyser db $
            GS.getTip >>= analyseBlockchain cli
  where
    conf = CLI.configurationOptions commonArgs
