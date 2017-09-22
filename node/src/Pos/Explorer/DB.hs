{-# LANGUAGE TypeFamilies #-}

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
       , sanityCheckBalances
       ) where

import           Universum

import           Control.Lens                 (at, non)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Conduit                 (Sink, Source, mapOutput, runConduitRes,
                                               (.|))
import qualified Data.Conduit.List            as CL
import qualified Database.RocksDB             as Rocks
import           Formatting                   (sformat, (%))
import           Serokell.Util                (Color (Red), colorize, mapJson)
import           System.Wlog                  (WithLogger, logError)

import           Pos.Binary.Class             (UnsignedVarInt (..), serialize')
import           Pos.Context.Functions        (genesisUtxo)
import           Pos.Core                     (Address, Coin, EpochIndex, HeaderHash,
                                               unsafeAddCoin)
import           Pos.Core.Configuration       (HasConfiguration)
import           Pos.DB                       (DBError (..), DBIteratorClass (..),
                                               DBTag (GStateDB), MonadDB,
                                               MonadDBRead (dbGet), RocksBatchOp (..),
                                               dbIterSource, dbSerializeValue,
                                               encodeWithKeyPrefix)
import           Pos.DB.GState.Common         (gsGetBi, gsPutBi, writeBatchGState)
import           Pos.Explorer.Core            (AddrHistory, TxExtra (..))
import           Pos.Txp.Core                 (Tx, TxId, TxOut (..), TxOutAux (..))
import           Pos.Txp.DB                   (getAllPotentiallyHugeUtxo, utxoSource)
import           Pos.Txp.Toil                 (GenesisUtxo (..), utxoF,
                                               utxoToAddressCoinPairs)
import           Pos.Util.Chrono              (NewestFirst (..))

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
                 gsGetBi . addrHistoryKey

getAddrBalance :: MonadDBRead m => Address -> m (Maybe Coin)
getAddrBalance = gsGetBi . addrBalanceKey

getPageBlocks :: MonadDBRead m => Page -> m (Maybe [HeaderHash])
getPageBlocks = gsGetBi . blockPagePrefix

getEpochBlocks :: MonadDBRead m => Epoch -> m (Maybe [HeaderHash])
getEpochBlocks = gsGetBi . blockEpochPrefix

getLastTransactions :: MonadDBRead m => m (Maybe [Tx])
getLastTransactions = gsGetBi lastTxsPrefix

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareExplorerDB :: (MonadReader ctx m, MonadDB m) => m ()
prepareExplorerDB =
    unlessM areBalancesInitialized $ do
        putGenesisBalances genesisUtxo
        putInitFlag

balancesInitFlag :: ByteString
balancesInitFlag = "e/init/"

areBalancesInitialized :: MonadDBRead m => m Bool
areBalancesInitialized = isJust <$> dbGet GStateDB balancesInitFlag

putInitFlag :: MonadDB m => m ()
putInitFlag = gsPutBi balancesInitFlag True

putGenesisBalances :: MonadDB m => GenesisUtxo -> m ()
putGenesisBalances (GenesisUtxo utxo) = writeBatchGState putAddrBalancesOp
  where
    putAddrBalancesOp :: [ExplorerOp]
    putAddrBalancesOp = map (uncurry PutAddrBalance) addressCoinsPairs

    addressCoinsPairs :: [(Address, Coin)]
    addressCoinsPairs = utxoToAddressCoinPairs utxo

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

instance HasConfiguration => RocksBatchOp ExplorerOp where

    toBatchOp (AddTxExtra id extra) =
        [Rocks.Put (txExtraPrefix id) (dbSerializeValue extra)]
    toBatchOp (DelTxExtra id) =
        [Rocks.Del $ txExtraPrefix id]

    toBatchOp (PutPageBlocks page pageBlocks) =
        [Rocks.Put (blockPagePrefix page) (dbSerializeValue pageBlocks)]

    toBatchOp (PutEpochBlocks epoch pageBlocks) =
        [Rocks.Put (blockEpochPrefix epoch) (dbSerializeValue pageBlocks)]

    toBatchOp (PutLastTxs lastTxs) =
        [Rocks.Put lastTxsPrefix (dbSerializeValue lastTxs)]

    toBatchOp (UpdateAddrHistory addr txs)
        | null txs = [Rocks.Del key]
        | otherwise = [Rocks.Put key (dbSerializeValue txs)]
      where
        key = addrHistoryKey addr

    toBatchOp (PutAddrBalance addr coin) =
        [Rocks.Put (addrBalanceKey addr) (dbSerializeValue coin)]
    toBatchOp (DelAddrBalance addr) =
        [Rocks.Del $ addrBalanceKey addr]

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

data BalancesIter

instance DBIteratorClass BalancesIter where
    type IterKey BalancesIter = Address
    type IterValue BalancesIter = Coin
    iterKeyPrefix = addrBalancePrefix

-- 'Source' corresponding to the whole balances mapping (for all addresses).
balancesSource :: (MonadDBRead m) => Source (ResourceT m) (Address, Coin)
balancesSource = dbIterSource GStateDB (Proxy @BalancesIter)

-- 'Sink' to turn balances source to a map.
balancesSink :: (MonadDBRead m) => Sink (Address, Coin) m (HashMap Address Coin)
balancesSink =
    CL.fold
        (\res (addr, coin) -> res & at addr . non minBound %~ unsafeAddCoin coin)
        mempty

----------------------------------------------------------------------------
-- Sanity check
----------------------------------------------------------------------------

-- | Check that balances stored in the Explorer DB are the same as the
-- balances computed from Utxo DB.
--
-- WARNING: this is potentially expensive operation, it shouldn't be
-- used in production.
sanityCheckBalances
    :: (MonadDBRead m, WithLogger m)
    => m ()
sanityCheckBalances = do
    let utxoBalancesSource =
            mapOutput ((txOutAddress &&& txOutValue) . toaOut . snd) utxoSource
    storedMap <- runConduitRes $ balancesSource .| balancesSink
    computedFromUtxoMap <- runConduitRes $ utxoBalancesSource .| balancesSink
    let fmt =
            ("Explorer's balances are inconsistent with UTXO.\nExplorer stores: "
             %mapJson%".\nUtxo version is: "%mapJson%"\n")
    let msg = sformat fmt storedMap computedFromUtxoMap
    unless (storedMap == computedFromUtxoMap) $ do
        logError $ colorize Red msg
        logError . colorize Red . sformat ("Actual utxo is: " %utxoF) =<<
            getAllPotentiallyHugeUtxo
        throwM $ DBMalformed msg

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

txExtraPrefix :: TxId -> ByteString
txExtraPrefix h = "e/tx/" <> serialize' h

addrHistoryKey :: Address -> ByteString
addrHistoryKey addr = "e/ah/" <> serialize' addr

addrBalancePrefix :: ByteString
addrBalancePrefix = "e/ab/"

addrBalanceKey :: Address -> ByteString
addrBalanceKey = encodeWithKeyPrefix @BalancesIter

blockPagePrefix :: Page -> ByteString
blockPagePrefix page = "e/page/" <> encodedPage
  where
    encodedPage = serialize' $ UnsignedVarInt page

blockEpochPrefix :: Epoch -> ByteString
blockEpochPrefix epoch = "e/epoch/" <> serialize' epoch

lastTxsPrefix :: ByteString
lastTxsPrefix = "e/ltxs/"
