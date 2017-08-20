-- | Module containing explorer-specific logic and data

module Pos.Explorer.DB
       ( ExplorerOp (..)
       , Page
       , Epoch
       , numOfLastTxs
       , getTxExtra
       , getAddrHistory
       , getAddrBalance
       , getPageBlocks
       , getEpochBlocks
       , getLastTransactions
       , prepareExplorerDB
       ) where

import           Universum

import qualified Database.RocksDB      as Rocks
import           Ether.Internal        (HasLens (..))

import           Pos.Binary.Class      (UnsignedVarInt (..), serialize')
import           Pos.Context.Functions (GenesisUtxo, genesisUtxoM)
import           Pos.Core.Types        (Address, Coin, EpochIndex, HeaderHash)
import           Pos.DB                (DBTag (GStateDB), MonadDB, MonadDBRead (dbGet),
                                        RocksBatchOp (..))
import           Pos.DB.GState.Common  (gsGetBi, gsPutBi, writeBatchGState)
import           Pos.Explorer.Core     (AddrHistory, TxExtra (..))
import           Pos.Txp.Core          (Tx, TxId)
import           Pos.Txp.Toil          (GenesisUtxo (..), utxoToAddressCoinPairs)
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

-- TODO: In time if we have enough constants, maybe add to explorer Constants?
numOfLastTxs :: Int
numOfLastTxs = 20

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

getLastTransactions :: MonadDBRead m => m (Maybe [Tx])
getLastTransactions = gsGetBi lastTxsPrefix

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

putGenesisBalances :: MonadDB m => GenesisUtxo -> m ()
putGenesisBalances (GenesisUtxo genesisUtxo) = writeBatchGState putAddrBalancesOp
  where
    putAddrBalancesOp :: [ExplorerOp]
    putAddrBalancesOp = map (uncurry PutAddrBalance) addressCoinsPairs

    addressCoinsPairs :: [(Address, Coin)]
    addressCoinsPairs = utxoToAddressCoinPairs genesisUtxo

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data ExplorerOp
    = AddTxExtra !TxId !TxExtra
    | DelTxExtra !TxId

    | PutPageBlocks !Page ![HeaderHash]

    | PutEpochBlocks !Epoch ![HeaderHash]

    | PutLastTxs ![Tx]

    | UpdateAddrHistory !Address !AddrHistory

    | PutAddrBalance !Address !Coin
    | DelAddrBalance !Address

instance RocksBatchOp ExplorerOp where

    toBatchOp (AddTxExtra id extra) =
        [Rocks.Put (txExtraPrefix id) (serialize' extra)]
    toBatchOp (DelTxExtra id) =
        [Rocks.Del $ txExtraPrefix id]

    toBatchOp (PutPageBlocks page pageBlocks) =
        [Rocks.Put (blockPagePrefix page) (serialize' pageBlocks)]

    toBatchOp (PutEpochBlocks epoch pageBlocks) =
        [Rocks.Put (blockEpochPrefix epoch) (serialize' pageBlocks)]

    toBatchOp (PutLastTxs lastTxs) =
        [Rocks.Put lastTxsPrefix (serialize' lastTxs)]

    toBatchOp (UpdateAddrHistory addr txs) =
        [Rocks.Put (addrHistoryPrefix addr) (serialize' txs)]

    toBatchOp (PutAddrBalance addr coin) =
        [Rocks.Put (addrBalancePrefix addr) (serialize' coin)]
    toBatchOp (DelAddrBalance addr) =
        [Rocks.Del $ addrBalancePrefix addr]

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

txExtraPrefix :: TxId -> ByteString
txExtraPrefix h = "e/tx/" <> serialize' h

addrHistoryPrefix :: Address -> ByteString
addrHistoryPrefix addr = "e/ah/" <> serialize' addr

addrBalancePrefix :: Address -> ByteString
addrBalancePrefix addr = "e/ab/" <> serialize' addr

blockPagePrefix :: Page -> ByteString
blockPagePrefix page = "e/page/" <> encodedPage
  where
    encodedPage = serialize' $ UnsignedVarInt page

blockEpochPrefix :: Epoch -> ByteString
blockEpochPrefix epoch = "e/epoch/" <> serialize' epoch

lastTxsPrefix :: ByteString
lastTxsPrefix = "e/ltxs/"
