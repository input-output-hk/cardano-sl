-- | Higher-level DB functionality.

module Pos.DB.DB
       ( openNodeDBs
       , getTipBlock
       , getTipBlockHeader
       , loadBlocksFromTipWhile
       ) where

import           Control.Monad.State          (get)
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.HashMap.Strict          as HM
import           Data.List.NonEmpty           (nonEmpty)
import qualified Data.Map.Strict              as M
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              ((</>))
import           Universum

import           Pos.DB.Block                 (getBlock, loadBlocksWithUndoWhile,
                                               prepareBlockDB)
import           Pos.DB.Class                 (MonadDB)
import           Pos.DB.Error                 (DBError (DBMalformed))
import           Pos.DB.Functions             (openDB)
import           Pos.DB.Holder                (runDBHolder)
import           Pos.DB.Misc                  (prepareMiscDB)
import           Pos.DB.Types                 (NodeDBs (..))
import           Pos.DB.Utxo                  (getTip, prepareUtxoDB)
import           Pos.Genesis                  (genesisLeaders)
import           Pos.Ssc.Class.Types          (Ssc)
import           Pos.Types                    (Address, Block, BlockHeader, Coin,
                                               Participants, TxOut (..), Undo, Utxo,
                                               getBlockHeader, headerHash, mkGenesisBlock)

-- TODO: copy-pasted from Worker.Lrc :(
getRichmen :: Utxo -> Participants
getRichmen =
    fromMaybe onNoRichmen .
    nonEmpty .
    HM.keys .
    HM.filter (>= threshold) . flip execState mempty . mapM countMoneys . M.toList
  where
    threshold = 0 -- TODO
    onNoRichmen = panic "There are no richmen!"
    countMoneys :: (a, TxOut) -> State (HM.HashMap Address Coin) ()
    countMoneys (_, TxOut {..}) = do
        money <- get
        let val = HM.lookupDefault 0 txOutAddress money
        modify (HM.insert txOutAddress (val + txOutValue))

-- | Open all DBs stored on disk.
openNodeDBs :: (Ssc ssc, MonadResource m) => FilePath -> Utxo -> m (NodeDBs ssc)
openNodeDBs fp customUtxo = do
    let blockPath = fp </> "blocks"
    let utxoPath = fp </> "utxo"
    let miscPath = fp </> "misc"
    mapM_ ensureDirectoryExists [blockPath, utxoPath, miscPath]
    res <- NodeDBs <$> openDB blockPath <*> openDB utxoPath <*> openDB miscPath
    let prepare = do
          prepareBlockDB genesisBlock0
          prepareUtxoDB customUtxo initialTip
          prepareMiscDB leaders0 richmen0
    res <$ runDBHolder res prepare
  where
    leaders0 = genesisLeaders customUtxo
    richmen0 = getRichmen customUtxo
    ensureDirectoryExists
        :: MonadIO m
        => FilePath -> m ()
    ensureDirectoryExists = liftIO . createDirectoryIfMissing True
    genesisBlock0 = mkGenesisBlock Nothing 0 leaders0
    initialTip = headerHash genesisBlock0

-- | Get block corresponding to tip.
getTipBlock
    :: (Ssc ssc, MonadDB ssc m)
    => m (Block ssc)
getTipBlock = maybe onFailure pure =<< getBlock =<< getTip
  where
    onFailure = throwM $ DBMalformed "there is no block corresponding to tip"

-- | Get BlockHeader corresponding to tip.
getTipBlockHeader
    :: (Ssc ssc, MonadDB ssc m)
    => m (BlockHeader ssc)
getTipBlockHeader = getBlockHeader <$> getTipBlock

-- | Load blocks from BlockDB starting from tip and while @condition@ is true.
-- The head of returned list is the youngest block.
loadBlocksFromTipWhile :: (Ssc ssc, MonadDB ssc m) => (Block ssc -> Bool) -> m [(Block ssc, Undo)]
loadBlocksFromTipWhile condition = getTip >>= flip loadBlocksWithUndoWhile condition
