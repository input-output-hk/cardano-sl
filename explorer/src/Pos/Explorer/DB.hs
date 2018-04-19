{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module containing explorer-specific logic and data

module Pos.Explorer.DB
       ( ExplorerOp (..)
       , Page
       , Epoch
       , EpochPagedBlocksKey
       , numOfLastTxs
       , defaultPageSize
       , getTxExtra
       , getAddrHistory
       , getAddrBalance
       , getUtxoSum
       , getPageBlocks
       , getEpochBlocks
       , getEpochPages
       , getLastTransactions
       , explorerInitDB
       , sanityCheckBalances
       -- * For testing
       , convertToPagedMap
       , findEpochMaxPages
       ) where

import           Universum

import           Control.Lens (at, non)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Conduit (ConduitT, mapOutput, runConduitRes, (.|))
import qualified Data.Conduit.List as CL
import           Data.List (groupBy)
import           Data.Map (fromList)
import qualified Data.Map as M
import qualified Database.RocksDB as Rocks
import           Formatting (sformat, (%))
import           Serokell.Util (Color (Red), colorize, mapJson)
import           System.Wlog (WithLogger, logError)
import           UnliftIO (MonadUnliftIO)

import           Pos.Binary.Class (serialize')
import           Pos.Core (Address, Coin, EpochIndex (..), HasConfiguration, HeaderHash,
                           coinToInteger, unsafeAddCoin)
import           Pos.Core.Txp (Tx, TxId, TxOut (..), TxOutAux (..))
import           Pos.DB (DBError (..), DBIteratorClass (..), DBTag (GStateDB), MonadDB,
                         MonadDBRead (dbGet), RocksBatchOp (..), dbIterSource, dbSerializeValue,
                         encodeWithKeyPrefix)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.GState.Common (gsGetBi, gsPutBi, writeBatchGState)
import           Pos.Explorer.Core (AddrHistory, TxExtra (..))
import           Pos.Ssc (HasSscConfiguration)
import           Pos.Txp.DB (getAllPotentiallyHugeUtxo, utxoSource)
import           Pos.Txp.GenesisUtxo (genesisUtxo)
import           Pos.Txp.Toil (GenesisUtxo (..), utxoF, utxoToAddressCoinPairs)
import           Pos.Util.Chrono (NewestFirst (..))
import           Pos.Util.Util (maybeThrow)



explorerInitDB
    :: forall ctx m.
       ( MonadReader ctx m
       , MonadUnliftIO m
       , MonadDB m
       , HasConfiguration
       , HasSscConfiguration
       )
    => m ()
explorerInitDB = initNodeDBs >> prepareExplorerDB

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

-- Left like type aliases in order to remain flexible.
type Page = Int
type Epoch = EpochIndex

type EpochPagedBlocksKey = (Epoch, Page)

-- TODO: In time if we have enough constants, maybe add to explorer Constants?
numOfLastTxs :: Int
numOfLastTxs = 20

-- | The default page size.
defaultPageSize :: Int
defaultPageSize = 10

----------------------------------------------------------------------------
-- Util
----------------------------------------------------------------------------

-- Find max pages for each epoch.
findEpochMaxPages :: M.Map EpochPagedBlocksKey [HeaderHash] -> [EpochPagedBlocksKey]
findEpochMaxPages epochPagedBlocksMap' =
    maximumBy (comparing snd) <$> groupedEpochPagedBlocks
  where
    groupedEpochPagedBlocks :: [[EpochPagedBlocksKey]]
    groupedEpochPagedBlocks =
        groupBy (\(epoch1,_) (epoch2,_) -> epoch1 == epoch2) epochPagedBlocksMapKeys

    epochPagedBlocksMapKeys :: [EpochPagedBlocksKey]
    epochPagedBlocksMapKeys = M.keys epochPagedBlocksMap'

-- | Convert a pair of @Epoch@ and a list of @HeaderHash@ to a paged @Map@ containing
-- @(Epoch, Page)@ as a key and a list of @HeaderHash@ as values.
convertToPagedMap
    :: (Epoch, [HeaderHash])
    -> Map EpochPagedBlocksKey [HeaderHash]
convertToPagedMap ehh = fromList $ convertToPaged ehh
  where
    convertToPaged :: (Epoch, [HeaderHash]) -> [((Epoch, Page), [HeaderHash])]
    convertToPaged (epoch, headerHashes) =
        map convertHHsToEpochPages convertHHsToPages
      where
        convertHHsToEpochPages
            :: (Page, [HeaderHash])
            -> ((Epoch, Page), [HeaderHash])
        convertHHsToEpochPages (page, headerHashes') =
            ((epoch, page), headerHashes')

        -- | Take that huge chunk of @HeaderHash@es and page it.
        convertHHsToPages :: [(Page, [HeaderHash])]
        convertHHsToPages = zip [1..] $ splitEvery defaultPageSize headerHashes
          where
            -- | Split the list every N elements.
            splitEvery :: Int -> [a] -> [[a]]
            splitEvery _ [] = []
            splitEvery n xs = as : splitEvery n bs
              where
                (as,bs) = splitAt n xs

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

getUtxoSum :: MonadDBRead m => m Integer
getUtxoSum = maybeThrow dbNotInitialized =<< gsGetBi utxoSumPrefix
  where
    dbNotInitialized = DBMalformed "getUtxoSum: DB is not initialized"

getPageBlocks :: MonadDBRead m => Page -> m (Maybe [HeaderHash])
getPageBlocks = gsGetBi . blockPagePrefix

getEpochBlocks :: MonadDBRead m => Epoch -> Page -> m (Maybe [HeaderHash])
getEpochBlocks epoch page = gsGetBi $ blockEpochPagePrefix epoch page

getEpochPages :: MonadDBRead m => Epoch -> m (Maybe Page)
getEpochPages = gsGetBi . blockEpochMaxPagePrefix

getLastTransactions :: MonadDBRead m => m (Maybe [Tx])
getLastTransactions = gsGetBi lastTxsPrefix

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareExplorerDB :: (MonadDB m, MonadUnliftIO m, HasConfiguration) => m ()
prepareExplorerDB = do
    unlessM balancesInitializedM $ do
        let GenesisUtxo utxo = genesisUtxo
            addressCoinPairs = utxoToAddressCoinPairs utxo
        putGenesisBalances addressCoinPairs
        putInitFlag

    -- Smooth migration for CSE-228.
    unlessM utxoSumInitializedM $ do
        putCurrentUtxoSum

    -- Smooth migration for CSE-236.
    unlessM blockEpochPagesInitializedM $ do
        convertOldEpochBlocksFormat
  where
    -- | If we have persisted the 0 @Epoch@ with the old format, we need to migrate it.
    -- Since we start syncing from the start, this will detect if the user has the old
    -- or the new format.
    -- Returns @True@ when the old format doesn't exist.
    blockEpochPagesInitializedM :: MonadDBRead m => m Bool
    blockEpochPagesInitializedM = isNothing <$> dbGet GStateDB existingOldEpochBlocks
      where
        existingOldEpochBlocks :: ByteString
        existingOldEpochBlocks = oldEpochBlocksPrefix $ EpochIndex minBound

    -- | This is where we convert the old format of @Epoch@ which has 21600 blocks to the
    -- new and shiny one with @Page@s added.
    convertOldEpochBlocksFormat :: (MonadDB m, MonadUnliftIO m) => m ()
    convertOldEpochBlocksFormat =
        runConduitRes  $ epochsSource
                      .| epochPagesConduit
                      .| epochPagesExplorerOpSink
      where
        -- 'Source' corresponding to the old @Epoch@ format that contained all
        -- 21600 @[HeaderHash]@ in a single @Epoch@ (in production).
        epochsSource :: (MonadDBRead m) => ConduitT () (Epoch, [HeaderHash]) (ResourceT m) ()
        epochsSource = dbIterSource GStateDB (Proxy @EpochsIter)

        -- 'Conduit' to turn the old @Epoch@ format that contained all
        -- 21600 @[HeaderHash]@ in a single @Epoch@ to a new format that contains
        -- @Epoch@ *and* @Page@ as a key since there are too many @[HeaderHash]@ to
        -- load - performance reasons.
        epochPagesConduit
            :: (MonadDBRead m)
            => ConduitT (Epoch, [HeaderHash]) (Map EpochPagedBlocksKey [HeaderHash]) m ()
        epochPagesConduit = CL.map convertToPagedMap

        -- | Finally, we persist the map with the new format.
        epochPagesExplorerOpSink
            :: (MonadDB m)
            => ConduitT (Map EpochPagedBlocksKey [HeaderHash]) Void m ()
        epochPagesExplorerOpSink = CL.mapM_ persistEpochBlocks
          where
            -- | Persist atomically, all the operations together.
            persistEpochBlocks
                :: (MonadDB m)
                => Map EpochPagedBlocksKey [HeaderHash]
                -> m ()
            persistEpochBlocks mapEpochPagedHHs =
                -- Here we persist all the changes atomically.
                writeBatchGState $ persistEpochPageBlocks ++ persistMaxPageNumbers
              where
                -- | Persist @Epoch@ @Page@ blocks.
                persistEpochPageBlocks :: [ExplorerOp]
                persistEpochPageBlocks = putKeyBlocks <$> M.toList mapEpochPagedHHs
                  where
                    putKeyBlocks
                        :: (EpochPagedBlocksKey, [HeaderHash])
                        -> ExplorerOp
                    putKeyBlocks keyBlocks = PutEpochBlocks epoch page blocks
                      where
                        key           = keyBlocks ^. _1
                        epoch         = key ^. _1
                        page          = key ^. _2

                        blocks        = keyBlocks ^. _2

                -- | Persist @Epoch@ max @Page@.
                persistMaxPageNumbers :: [ExplorerOp]
                persistMaxPageNumbers =
                    [ PutEpochPages epoch maxPageNumber | (epoch, maxPageNumber) <- emp]
                  where
                    emp :: [EpochPagedBlocksKey]
                    emp = findEpochMaxPages mapEpochPagedHHs


balancesInitFlag :: ByteString
balancesInitFlag = "e/init/"

balancesInitializedM :: MonadDBRead m => m Bool
balancesInitializedM = isJust <$> dbGet GStateDB balancesInitFlag

putInitFlag :: MonadDB m => m ()
putInitFlag = gsPutBi balancesInitFlag True

putGenesisBalances :: (MonadDB m, HasConfiguration) => [(Address, Coin)] -> m ()
putGenesisBalances addressCoinPairs = writeBatchGState putAddrBalancesOp
  where
    putAddrBalancesOp :: [ExplorerOp]
    putAddrBalancesOp = map (uncurry PutAddrBalance) addressCoinPairs

utxoSumInitializedM :: MonadDBRead m => m Bool
utxoSumInitializedM = isJust <$> dbGet GStateDB utxoSumPrefix

putCurrentUtxoSum :: (MonadDB m, MonadUnliftIO m, HasConfiguration) => m ()
putCurrentUtxoSum = do
    utxoSum <- computeUtxoSum
    writeBatchGState [PutUtxoSum utxoSum]
  where
    computeUtxoSum :: (MonadDBRead m, MonadUnliftIO m) => m Integer
    computeUtxoSum = do
        let txOutValueSource =
                mapOutput (coinToInteger . txOutValue . toaOut . snd) utxoSource
        runConduitRes $ txOutValueSource .| CL.fold (+) 0

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data ExplorerOp
    = AddTxExtra !TxId !TxExtra
    | DelTxExtra !TxId

    | PutPageBlocks !Page ![HeaderHash]

    | PutEpochBlocks !Epoch !Page ![HeaderHash]
    | PutEpochPages !Epoch !Page

    | PutLastTxs ![Tx]

    | UpdateAddrHistory !Address !AddrHistory

    | PutAddrBalance !Address !Coin
    | DelAddrBalance !Address

    | PutUtxoSum !Integer

instance HasConfiguration => RocksBatchOp ExplorerOp where
    toBatchOp (AddTxExtra id extra) =
        [Rocks.Put (txExtraPrefix id) (dbSerializeValue extra)]
    toBatchOp (DelTxExtra id) =
        [Rocks.Del $ txExtraPrefix id]

    toBatchOp (PutPageBlocks page pageBlocks) =
        [Rocks.Put (blockPagePrefix page) (dbSerializeValue pageBlocks)]

    toBatchOp (PutEpochBlocks epoch page pageBlocks) =
        [Rocks.Put (blockEpochPagePrefix epoch page) (dbSerializeValue pageBlocks)]
    toBatchOp (PutEpochPages epoch page) =
        [Rocks.Put (blockEpochMaxPagePrefix epoch) (dbSerializeValue page)]

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

    toBatchOp (PutUtxoSum utxoSum) =
        [Rocks.Put utxoSumPrefix (dbSerializeValue utxoSum)]

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

----------------------------------------------------------------------------
--- Balances
----------------------------------------------------------------------------

data BalancesIter

instance DBIteratorClass BalancesIter where
    type IterKey BalancesIter = Address
    type IterValue BalancesIter = Coin
    iterKeyPrefix = addrBalancePrefix

-- 'Source' corresponding to the whole balances mapping (for all addresses).
balancesSource :: (MonadDBRead m) => ConduitT () (Address, Coin) (ResourceT m) ()
balancesSource = dbIterSource GStateDB (Proxy @BalancesIter)

-- 'Sink' to turn balances source to a map.
balancesSink :: (MonadDBRead m) => ConduitT (Address, Coin) Void m (HashMap Address Coin)
balancesSink =
    CL.fold
        (\res (addr, coin) -> res & at addr . non minBound %~ unsafeAddCoin coin)
        mempty

----------------------------------------------------------------------------
--- Epochs
----------------------------------------------------------------------------

data EpochsIter

instance DBIteratorClass EpochsIter where
    type IterKey EpochsIter = Epoch
    type IterValue EpochsIter = [HeaderHash]
    iterKeyPrefix = "e/epoch/"

----------------------------------------------------------------------------
-- Sanity check
----------------------------------------------------------------------------

-- | Check that balances stored in the Explorer DB are the same as the
-- balances computed from Utxo DB.
--
-- WARNING: this is potentially expensive operation, it shouldn't be
-- used in production.
sanityCheckBalances
    :: (MonadDBRead m, WithLogger m, MonadUnliftIO m)
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
    encodedPage = serialize' page

-- | TODO(ks): To remove in the next version.
oldEpochBlocksPrefix :: Epoch -> ByteString
oldEpochBlocksPrefix epoch = "e/epoch/" <> serialize' epoch

-- | Before we had - @oldEpochBlocksPrefix@.
blockEpochPagePrefix :: Epoch -> Page -> ByteString
blockEpochPagePrefix epoch page = "e/epochs/" <> serialize' epoch <> "/" <> encodedPage
  where
    encodedPage = serialize' page

blockEpochMaxPagePrefix :: Epoch -> ByteString
blockEpochMaxPagePrefix epoch = "e/epochPages/" <> serialize' epoch

lastTxsPrefix :: ByteString
lastTxsPrefix = "e/ltxs/"

utxoSumPrefix :: ByteString
utxoSumPrefix = "e/utxosum/"
