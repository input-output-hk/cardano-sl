-- | Module containing explorer-specific logic and data

module Pos.Explorer.DB
       ( ExplorerOp (..)
       , Page
       , Epoch
       , getTxExtra
       , getAddrHistory
       , getAddrBalance
       , getPageBlocks
       , getEpochBlocks
       , prepareExplorerDB
       ) where

import           Universum

import qualified Data.HashMap.Strict   as HM
import qualified Data.Map.Strict       as M
import qualified Database.RocksDB      as Rocks
import           Ether.Internal        (HasLens (..))

import           Pos.Binary.Class      (UnsignedVarInt (..), encode)
import           Pos.Context.Functions (GenesisUtxo, genesisUtxoM)
import           Pos.Core              (unsafeAddCoin)
import           Pos.Core.Types        (Address, Coin, HeaderHash, EpochIndex)
import           Pos.DB                (DBTag (GStateDB), MonadDB, MonadDBRead (dbGet),
                                        RocksBatchOp (..))
import           Pos.DB.GState.Common  (gsGetBi, gsPutBi, writeBatchGState)
import           Pos.Explorer.Core     (AddrHistory, TxExtra (..))
import           Pos.Txp.Core          (TxId, TxOutAux (..), _TxOut)
import           Pos.Txp.Toil          (Utxo)
import           Pos.Util.Chrono       (NewestFirst (..))

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

-- Left like type aliases in order to remain flexible.
type Page = Int
type Epoch = EpochIndex

-- type PageBlocks = [Block SscGodTossing]
-- ^ this is much simpler but we are trading time for space 
-- (since space is an issue, it seems)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getTxExtra :: MonadDBRead m => TxId -> m (Maybe TxExtra)
getTxExtra = gsGetBi . txExtraPrefix

getAddrHistory :: MonadDBRead m => Address -> m AddrHistory
getAddrHistory = fmap (NewestFirst . concat . maybeToList) .
                 gsGetBi . addrHistoryPrefix

getAddrBalance :: MonadDBRead m => Address -> m (Maybe Coin)
getAddrBalance = gsGetBi . addrBalancePrefix

getPageBlocks :: MonadDBRead m => Page -> m (Maybe [HeaderHash])
getPageBlocks = gsGetBi . blockPagePrefix

getEpochBlocks :: MonadDBRead m => Epoch -> m (Maybe [HeaderHash])
getEpochBlocks = gsGetBi . blockEpochPrefix

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareExplorerDB :: (MonadReader ctx m, HasLens GenesisUtxo ctx GenesisUtxo, MonadDB m) => m ()
prepareExplorerDB =
    unlessM areBalancesInitialized $ do
        genesisUtxo <- genesisUtxoM
        putGenesisBalances genesisUtxo
        putInitFlag

balancesInitFlag :: ByteString
balancesInitFlag = "e/init/"

areBalancesInitialized :: MonadDBRead m => m Bool
areBalancesInitialized = isJust <$> dbGet GStateDB balancesInitFlag

putInitFlag :: MonadDB m => m ()
putInitFlag = gsPutBi balancesInitFlag True

putGenesisBalances :: MonadDB m => Utxo -> m ()
putGenesisBalances genesisUtxo =
    writeBatchGState $
    map (uncurry PutAddrBalance) $ combineWith unsafeAddCoin txOuts
  where
    txOuts = map (view _TxOut . toaOut) . M.elems $ genesisUtxo
    combineWith :: (Eq a, Hashable a) => (b -> b -> b) -> [(a, b)] -> [(a, b)]
    combineWith func = HM.toList . HM.fromListWith func

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data ExplorerOp
    = AddTxExtra !TxId !TxExtra
    | DelTxExtra !TxId

    | PutPageBlocks !Page ![HeaderHash]

    | PutEpochBlocks !Epoch ![HeaderHash]

    | UpdateAddrHistory !Address !AddrHistory

    | PutAddrBalance !Address !Coin
    | DelAddrBalance !Address

instance RocksBatchOp ExplorerOp where
  
    toBatchOp (AddTxExtra id extra) =
        [Rocks.Put (txExtraPrefix id) (encode extra)]
    toBatchOp (DelTxExtra id) =
        [Rocks.Del $ txExtraPrefix id]
    
    toBatchOp (PutPageBlocks page pageBlocks) =
        [Rocks.Put (blockPagePrefix page) (encode pageBlocks)]

    toBatchOp (PutEpochBlocks epoch pageBlocks) =
        [Rocks.Put (blockEpochPrefix epoch) (encode pageBlocks)]

    toBatchOp (UpdateAddrHistory addr txs) =
        [Rocks.Put (addrHistoryPrefix addr) (encode txs)]

    toBatchOp (PutAddrBalance addr coin) =
        [Rocks.Put (addrBalancePrefix addr) (encode coin)]
    toBatchOp (DelAddrBalance addr) =
        [Rocks.Del $ addrBalancePrefix addr]

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

txExtraPrefix :: TxId -> ByteString
txExtraPrefix h = "e/tx/" <> encode h

addrHistoryPrefix :: Address -> ByteString
addrHistoryPrefix addr = "e/ah/" <> encode addr

addrBalancePrefix :: Address -> ByteString
addrBalancePrefix addr = "e/ab/" <> encode addr

blockPagePrefix :: Page -> ByteString
blockPagePrefix page = "e/page/" <> encodedPage
  where
    encodedPage = encode $ UnsignedVarInt page

blockEpochPrefix :: Epoch -> ByteString
blockEpochPrefix epoch = "e/epoch/" <> encode epoch
