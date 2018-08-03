{-# LANGUAGE AllowAmbiguousTypes #-}
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

       -- * History derivation
       , getBlockHistoryDefault
       , getLocalHistoryDefault
       , SaveTxException (..)
       , saveTxDefault

       , txHistoryListToMap
       ) where

import           Universum

import           Control.Exception.Safe (Exception (..))
import           Control.Lens (makeLenses)
import           Control.Monad.Trans (MonadTrans)
import qualified Data.Map.Strict as M (fromList, insert)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable
import           Serokell.Util.Text (listJson)
import           System.Wlog (WithLogger)

import           Pos.Chain.Block (Block, MainBlock, genesisBlock0, headerHash,
                     mainBlockSlot, mainBlockTxPayload)
import           Pos.Chain.Lrc (genesisLeaders)
import           Pos.Chain.Txp (ToilVerFailure, Tx (..), TxAux (..), TxId,
                     TxOut, TxOutAux (..), TxWitness, TxpConfiguration,
                     TxpError (..), UtxoLookup, UtxoM, UtxoModifier,
                     applyTxToUtxo, evalUtxoM, flattenTxPayload, genesisUtxo,
                     runUtxoM, topsortTxs, txOutAddress, unGenesisUtxo,
                     utxoGet, utxoToLookup)
import           Pos.Core (Address, ChainDifficulty, GenesisHash (..),
                     HasConfiguration, Timestamp (..), difficultyL, epochSlots,
                     genesisHash)
import           Pos.Core.JsonLog (CanJsonLog (..))
import           Pos.Crypto (ProtocolMagic, WithHash (..), withHash)
import           Pos.DB (MonadDBRead, MonadGState)
import           Pos.DB.Block (getBlock)
import           Pos.DB.Txp (MempoolExt, MonadTxpLocal, MonadTxpMem, buildUtxo,
                     getLocalTxs, txpProcessTx, withTxpLocalData)
import qualified Pos.GState as GS
import           Pos.Infra.Network.Types (HasNodeType)
import           Pos.Infra.Slotting (MonadSlots, getSlotStartPure,
                     getSystemStartM)
import           Pos.Infra.StateLock (StateLock, StateLockMetrics)
import           Pos.Infra.Util.JsonLog.Events (MemPoolModifyReason)
import           Pos.Util (eitherToThrow, maybeThrow)
import           Pos.Util.Util (HasLens')

----------------------------------------------------------------------
-- Deduction of history
----------------------------------------------------------------------

-- | For given tx, gives list of source addresses of this tx, with respective 'TxIn's
getSenders :: Tx -> UtxoM [TxOut]
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

instance NFData TxHistoryEntry where
    rnf tx = _thTxId tx
        `deepseq` _thTx tx
        `deepseq` _thDifficulty tx
        `deepseq` _thInputAddrs tx
        `deepseq` _thOutputAddrs tx
        `deepseq` _thTimestamp tx
        `deepseq` ()

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
    :: ([Address] -> Bool)
    -> Maybe ChainDifficulty
    -> Maybe Timestamp
    -> [(WithHash Tx, TxWitness)]
    -> UtxoM (Map TxId TxHistoryEntry)
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
    :: [Address]
    -> Maybe ChainDifficulty
    -> Maybe Timestamp
    -> [(WithHash Tx, TxWitness)]
    -> UtxoM (Map TxId TxHistoryEntry)
getRelatedTxsByAddrs addrs = getTxsByPredicate $ any (`elem` addrs)

deriveAddrHistoryBlk
    :: [Address]
    -> (MainBlock -> Maybe Timestamp)
    -> Map TxId TxHistoryEntry
    -> Block
    -> UtxoM (Map TxId TxHistoryEntry)
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
-- Genesis UtxoLookup
----------------------------------------------------------------------------

genesisUtxoLookup :: HasConfiguration => UtxoLookup
genesisUtxoLookup = utxoToLookup . unGenesisUtxo $ genesisUtxo

----------------------------------------------------------------------------
-- MonadTxHistory
----------------------------------------------------------------------------

-- | A class which have methods to get transaction history
class (Monad m, HasConfiguration) => MonadTxHistory m where
    getBlockHistory
        :: ProtocolMagic -> [Address] -> m (Map TxId TxHistoryEntry)
    getLocalHistory
        :: [Address] -> m (Map TxId TxHistoryEntry)
    saveTx :: ProtocolMagic -> TxpConfiguration -> (TxId, TxAux) -> m ()

    default getBlockHistory
        :: (MonadTrans t, MonadTxHistory m', t m' ~ m)
        => ProtocolMagic -> [Address] -> m (Map TxId TxHistoryEntry)
    getBlockHistory pm = lift . getBlockHistory pm

    default getLocalHistory
        :: (MonadTrans t, MonadTxHistory m', t m' ~ m)
        => [Address] -> m (Map TxId TxHistoryEntry)
    getLocalHistory = lift . getLocalHistory

    default saveTx
        :: (MonadTrans t, MonadTxHistory m', t m' ~ m)
        => ProtocolMagic
        -> TxpConfiguration
        -> (TxId, TxAux)
        -> m ()
    saveTx pm txpConfig = lift . saveTx pm txpConfig

instance {-# OVERLAPPABLE #-}
    (MonadTxHistory m, MonadTrans t, Monad (t m)) =>
        MonadTxHistory (t m)

type TxHistoryEnv ctx m =
    ( MonadDBRead m
    , MonadGState m
    , MonadTxpLocal m
    , MonadMask m
    , WithLogger m
    , MonadSlots ctx m
    , MonadReader ctx m
    , MonadTxpMem (MempoolExt m) ctx m
    , HasLens' ctx StateLock
    , HasLens' ctx (StateLockMetrics MemPoolModifyReason)
    , HasNodeType ctx
    , CanJsonLog m
    )

getBlockHistoryDefault
    :: forall ctx m
     . (HasConfiguration, TxHistoryEnv ctx m)
    => ProtocolMagic
    -> [Address]
    -> m (Map TxId TxHistoryEntry)
getBlockHistoryDefault pm addrs = do
    let bot      = headerHash (genesisBlock0 pm (GenesisHash genesisHash) (genesisLeaders epochSlots))
    sd          <- GS.getSlottingData
    systemStart <- getSystemStartM

    let getBlockTimestamp :: MainBlock -> Maybe Timestamp
        getBlockTimestamp blk =
            getSlotStartPure systemStart (blk ^. mainBlockSlot) sd

    let filterFunc _blk _depth = True

    let foldStep ::
               (Map TxId TxHistoryEntry, UtxoModifier)
            -> Block
            -> (Map TxId TxHistoryEntry, UtxoModifier)
        foldStep (hist, modifier) blk =
            runUtxoM
                modifier
                genesisUtxoLookup
                (deriveAddrHistoryBlk addrs getBlockTimestamp hist blk)

    fst <$> GS.foldlUpWhileM getBlock bot filterFunc (pure ... foldStep) mempty

getLocalHistoryDefault
    :: forall ctx m. TxHistoryEnv ctx m
    => [Address] -> m (Map TxId TxHistoryEntry)
getLocalHistoryDefault addrs = do
    let mapper (txid, TxAux {..}) = (WithHash taTx txid, taWitness)
        topsortErr =
            TxpInternalError
                "getLocalHistory: transactions couldn't be topsorted!"
    localTxs <- withTxpLocalData getLocalTxs
    let ltxs = map mapper localTxs
    topsorted <- maybeThrow topsortErr (topsortTxs (view _1) ltxs)
    utxoLookup <- utxoToLookup <$> buildUtxo mempty (map snd localTxs)
    return $
        evalUtxoM mempty utxoLookup $
        getRelatedTxsByAddrs addrs Nothing Nothing topsorted

data SaveTxException =
    SaveTxToilFailure !ToilVerFailure
    deriving (Show)

instance Exception SaveTxException where
    displayException =
        \case
            SaveTxToilFailure x -> toString (pretty x)

saveTxDefault :: TxHistoryEnv ctx m
              => ProtocolMagic
              -> TxpConfiguration
              -> (TxId, TxAux) -> m ()
saveTxDefault pm txpConfig txw = do
    res <- txpProcessTx pm txpConfig txw
    eitherToThrow (first SaveTxToilFailure res)

txHistoryListToMap :: [TxHistoryEntry] -> Map TxId TxHistoryEntry
txHistoryListToMap = M.fromList . map (\tx -> (_thTxId tx, tx))
