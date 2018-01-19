{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module Pos.Client.Txp.History
       ( TxHistoryEntry(..)
       , thTxId
       , thTx
       , thInputs
       , thDifficulty
       , thOutputAddrs
       , thTimestamp
       , _thInputAddrs

       , MonadTxHistory(..)

       , GenesisToil
       , runGenesisToil

       -- * History derivation
       , getRelatedTxsByAddrs
       , deriveAddrHistory
       , deriveAddrHistoryBlk
       , getBlockHistoryDefault
       , getLocalHistoryDefault
       , saveTxDefault

       , txHistoryListToMap
       ) where

import           Universum

import           Control.Lens                 (makeLenses)
import           Control.Monad.Trans          (MonadTrans)
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Data.Map.Strict              as M (fromList, insert, lookup)
import qualified Data.Text.Buildable
import qualified Ether
import           Formatting                   (bprint, build, (%))
import           Mockable                     (CurrentTime, Mockable)
import           Serokell.Util.Text           (listJson)
import           System.Wlog                  (WithLogger)

import           Pos.Block.Core               (Block, MainBlock, mainBlockSlot,
                                               mainBlockTxPayload)
import           Pos.Block.Types              (Blund)
import           Pos.Context                  (genesisBlock0)
import           Pos.Core                     (Address, ChainDifficulty, HasConfiguration,
                                               HeaderHash, Timestamp (..), difficultyL,
                                               headerHash)
import           Pos.Crypto                   (WithHash (..), withHash)
import           Pos.DB                       (MonadDBRead, MonadGState, MonadRealDB)
import           Pos.DB.Block                 (MonadBlockDB)
import qualified Pos.GState                   as GS
import           Pos.Reporting                (MonadReporting)
import           Pos.Slotting                 (MonadSlots, getSlotStartPure,
                                               getSystemStartM)
import           Pos.Ssc.Class                (SscHelpersClass)
import           Pos.StateLock                (StateLock, StateLockMetrics)
import           Pos.Util.Util                (HasLens')
#ifdef WITH_EXPLORER
import           Pos.Explorer.Txp.Local       (eTxProcessTransaction)
#else
import           Pos.Txp                      (txProcessTransaction)
#endif
import           Pos.Txp                      (MonadTxpMem, MonadUtxo, MonadUtxoRead,
                                               ToilT, Tx (..), TxAux (..), TxId, TxOut,
                                               TxOutAux (..), TxWitness, TxpError (..),
                                               applyTxToUtxo, evalToilTEmpty,
                                               flattenTxPayload, genesisUtxo, getLocalTxs,
                                               runDBToil, topsortTxs, txOutAddress,
                                               unGenesisUtxo, utxoGet)
import           Pos.Txp.MemState             (MemPoolSnapshot)
import           Pos.Util                     (eitherToThrow, maybeThrow)
import           Pos.WorkMode.Class           (TxpExtra_TMP)

-- Remove this once there's no #ifdef-ed Pos.Txp import
{-# ANN module ("HLint: ignore Use fewer imports" :: Text) #-}

----------------------------------------------------------------------
-- Deduction of history
----------------------------------------------------------------------

-- | For given tx, gives list of source addresses of this tx, with respective 'TxIn's
getSenders :: MonadUtxoRead m => Tx -> m [TxOut]
getSenders UnsafeTx {..} = do
    utxo <- catMaybes <$> mapM utxoGet (toList _txInputs)
    return $ toaOut <$> utxo

-- | Datatype for returning info about tx history
data TxHistoryEntry = THEntry
    { _thTxId        :: !TxId
    , _thTx          :: !Tx
    , _thDifficulty  :: !(Maybe ChainDifficulty)
    , _thInputs      :: ![TxOut]
    , _thOutputAddrs :: ![Address]
    , _thTimestamp   :: !(Maybe Timestamp)
    } deriving (Show, Eq, Generic, Ord)

-- | Remained for compatibility
_thInputAddrs :: TxHistoryEntry -> [Address]
_thInputAddrs = map txOutAddress . _thInputs

makeLenses ''TxHistoryEntry

instance Buildable TxHistoryEntry where
    build THEntry {..} =
        bprint
            ("{ id="%build%" inputs="%listJson%" outputs="%listJson
             %" diff="%build%" time="%build%" }")
            _thTxId
            _thInputs
            _thOutputAddrs
            _thDifficulty
            _thTimestamp

-- | Select transactions by predicate on related addresses
getTxsByPredicate
    :: MonadUtxo m
    => ([Address] -> Bool)
    -> Maybe ChainDifficulty
    -> Maybe Timestamp
    -> [(WithHash Tx, TxWitness)]
    -> m (Map TxId TxHistoryEntry)
getTxsByPredicate pr mDiff mTs txs = go txs mempty
  where
    go [] !acc = return acc
    go ((wh@(WithHash tx txId), _wit) : rest) !acc = do
        inputs <- getSenders tx
        let outgoings = toList $ txOutAddress <$> _txOutputs tx
        let incomings = map txOutAddress inputs

        applyTxToUtxo wh

        let acc' = if pr (incomings ++ outgoings)
                   then M.insert txId (THEntry txId tx mDiff inputs outgoings mTs) acc
                   else acc
        go rest acc'

-- | Select transactions related to one of given addresses
getRelatedTxsByAddrs
    :: MonadUtxo m
    => [Address]
    -> Maybe ChainDifficulty
    -> Maybe Timestamp
    -> [(WithHash Tx, TxWitness)]
    -> m (Map TxId TxHistoryEntry)
getRelatedTxsByAddrs addrs = getTxsByPredicate $ any (`elem` addrs)

-- | Given a full blockchain, derive address history and Utxo
-- TODO: Such functionality will still be useful for merging
-- blockchains when wallet state is ready, but some metadata for
-- Tx will be required.
deriveAddrHistory
    :: MonadUtxo m
    => [Address] -> [Block ssc] -> m (Map TxId TxHistoryEntry)
deriveAddrHistory addrs chain =
    foldrM (flip $ deriveAddrHistoryBlk addrs $ const Nothing) mempty chain

deriveAddrHistoryBlk
    :: MonadUtxo m
    => [Address]
    -> (MainBlock ssc -> Maybe Timestamp)
    -> Map TxId TxHistoryEntry
    -> Block ssc
    -> m (Map TxId TxHistoryEntry)
deriveAddrHistoryBlk _ _ hist (Left _) = pure hist
deriveAddrHistoryBlk addrs getTs hist (Right blk) = do
    let mapper TxAux {..} = (withHash taTx, taWitness)
        difficulty = blk ^. difficultyL
        mTimestamp = getTs blk
    txs <- getRelatedTxsByAddrs addrs (Just difficulty) mTimestamp $
           map mapper . flattenTxPayload $
           blk ^. mainBlockTxPayload
    return $ txs <> hist -- TODO: Are we sure there is no intersection? OTherwise, the order might matter

----------------------------------------------------------------------------
-- GenesisToil
----------------------------------------------------------------------------

-- | Identity wrapper to use genesis utxo in context as `MonadUtxoRead` instance
-- TODO: probably should be moved elsewhere; `Pos.Txp.Toil` is not possible, because
-- of dependency on `Pos.Context` from main package
data GenesisToilTag

type GenesisToil = Ether.TaggedTrans GenesisToilTag IdentityT

runGenesisToil :: GenesisToil m a -> m a
runGenesisToil = coerce

instance (Monad m, HasConfiguration) =>
         MonadUtxoRead (GenesisToil m) where
    utxoGet txIn = pure . M.lookup txIn . unGenesisUtxo $ genesisUtxo

----------------------------------------------------------------------------
-- MonadTxHistory
----------------------------------------------------------------------------

-- | A class which have methods to get transaction history
class (Monad m, SscHelpersClass ssc) => MonadTxHistory ssc m | m -> ssc where
    getBlockHistory
        :: SscHelpersClass ssc
        => [Address] -> m (Map TxId TxHistoryEntry)
    getLocalHistory
        :: MemPoolSnapshot -> [Address] -> m (Map TxId TxHistoryEntry)
    saveTx :: MemPoolSnapshot -> (TxId, TxAux) -> m ()

    default getBlockHistory
        :: (MonadTrans t, MonadTxHistory ssc m', t m' ~ m)
        => [Address] -> m (Map TxId TxHistoryEntry)
    getBlockHistory = lift . getBlockHistory

    default getLocalHistory
        :: (MonadTrans t, MonadTxHistory ssc m', t m' ~ m)
        => MemPoolSnapshot -> [Address] -> m (Map TxId TxHistoryEntry)
    getLocalHistory m = lift . getLocalHistory m

    default saveTx :: (MonadTrans t, MonadTxHistory ssc m', t m' ~ m) => MemPoolSnapshot -> (TxId, TxAux) -> m ()
    saveTx m = lift . saveTx m

instance {-# OVERLAPPABLE #-}
    (MonadTxHistory ssc m, MonadTrans t, Monad (t m)) =>
        MonadTxHistory ssc (t m)

type TxHistoryEnv ctx m =
    ( MonadRealDB ctx m
    , MonadDBRead m
    , MonadGState m
    , MonadMask m
    , WithLogger m
    , MonadSlots ctx m
    , MonadReader ctx m
    , MonadTxpMem TxpExtra_TMP ctx m
    , HasLens' ctx StateLock
    , HasLens' ctx StateLockMetrics
    , MonadBaseControl IO m
    , Mockable CurrentTime m
    , MonadReporting ctx m
    )

type TxHistoryEnv' ssc ctx m =
    ( MonadBlockDB ssc m
    , TxHistoryEnv ctx m
    )

type GenesisHistoryFetcher m = ToilT () (GenesisToil m)

getBlockHistoryDefault
    :: forall ssc ctx m. (HasConfiguration, SscHelpersClass ssc, TxHistoryEnv' ssc ctx m)
    => [Address] -> m (Map TxId TxHistoryEntry)
getBlockHistoryDefault addrs = do
    let bot      = headerHash (genesisBlock0 @ssc)
    sd          <- GS.getSlottingData
    systemStart <- getSystemStartM

    let fromBlund :: Blund ssc -> GenesisHistoryFetcher m (Block ssc)
        fromBlund = pure . fst

        getBlockTimestamp :: MainBlock ssc -> Maybe Timestamp
        getBlockTimestamp blk = getSlotStartPure systemStart (blk ^. mainBlockSlot) sd

        blockFetcher :: HeaderHash -> GenesisHistoryFetcher m (Map TxId TxHistoryEntry)
        blockFetcher start = GS.foldlUpWhileM fromBlund start (const $ const True)
            (deriveAddrHistoryBlk addrs getBlockTimestamp) mempty

    runGenesisToil . evalToilTEmpty $ blockFetcher bot

getLocalHistoryDefault
    :: forall ctx m. TxHistoryEnv ctx m
    => MemPoolSnapshot -> [Address] -> m (Map TxId TxHistoryEntry)
getLocalHistoryDefault mempoolSnapshot addrs = runDBToil . evalToilTEmpty $ do
    let mapper (txid, TxAux {..}) =
            (WithHash taTx txid, taWitness)
        topsortErr = TxpInternalError
            "getLocalHistory: transactions couldn't be topsorted!"
    let ltxs = map mapper (getLocalTxs mempoolSnapshot)
    txs <- getRelatedTxsByAddrs addrs Nothing Nothing =<<
           maybeThrow topsortErr (topsortTxs (view _1) ltxs)
    return $ txs

saveTxDefault :: TxHistoryEnv ctx m => MemPoolSnapshot -> (TxId, TxAux) -> m ()
saveTxDefault mps txw = do
#ifdef WITH_EXPLORER
    res <- eTxProcessTransaction mps txw
#else
    res <- txProcessTransaction mps txw
#endif
    eitherToThrow res

txHistoryListToMap :: [TxHistoryEntry] -> Map TxId TxHistoryEntry
txHistoryListToMap = M.fromList . map (\tx -> (_thTxId tx, tx))
