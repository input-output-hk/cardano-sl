{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Main where

import           Formatting
import           Mockable           (runProduction)
import           Pos.Block.Core     (GenesisBlock)
import           Pos.Core           (HasCoreConstants, gbHeader, gbhPrevBlock,
                                     giveStaticConsts, headerHash)
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

import           Options            (CLIOptions (..), getOptions)
import           Rendering          (render)
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
            analyseBlockchain tip

analyseBlockchain :: HasCoreConstants => HeaderHash -> BlockchainInspector ()
analyseBlockchain currentTip = do
    res <- DB.dbGetBlockSumDefault @SscGodTossing currentTip
    case res of
        Nothing -> putText "No tip found."
        Just (Right _) -> putText "end."
        Just (Left  gB) -> do
            liftIO $ putText (renderGenesisBlock gB)
            analyseBlockchain (view gbhPrevBlock (gB ^. gbHeader))


renderGenesisBlock :: HasCoreConstants => GenesisBlock SscGodTossing -> Text
renderGenesisBlock b = sformat build b

{--
-- | Block.
type Block ssc = Either (GenesisBlock ssc) (MainBlock ssc)

data GenericBlock b = UnsafeGenericBlock
    { _gbHeader :: !(GenericBlockHeader b)
    , _gbBody   :: !(Body b)
    , _gbExtra  :: !(ExtraBodyData b)
    } deriving (Generic)
--}


-- (OldestFirst [] (Blund SscGodTossing))
