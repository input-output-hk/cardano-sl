-- | Explorer's local Txp.

module Pos.Explorer.Txp.Local
       ( eTxProcessTransaction
       , eTxNormalize
       ) where

import           Universum

import           Control.Monad.Except        (MonadError (..))
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Default                (def)
import qualified Data.HashMap.Strict         as HM
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map                    as M (fromList)
import           Ether.Internal              (HasLens (..))
import           Formatting                  (build, sformat, (%))
import           System.Wlog                 (WithLogger, logDebug)

import           Pos.Core                    (BlockVersionData, EpochIndex, HeaderHash,
                                              Timestamp, siEpoch)
import           Pos.DB.Class                (MonadDBRead, MonadGState (..))
import qualified Pos.Explorer.DB             as ExDB
import qualified Pos.GState                  as GS
import           Pos.Slotting                (MonadSlots (currentTimeSlotting, getCurrentSlot))
import           Pos.Txp.Core                (Tx (..), TxAux (..), TxId, toaOut,
                                              txOutAddress)
import           Pos.Txp.MemState            (GenericTxpLocalDataPure, MonadTxpMem,
                                              getLocalTxsMap, getTxpExtra,
                                              getUtxoModifier, modifyTxpLocalData,
                                              setTxpLocalData)
import           Pos.Txp.Toil                (GenericToilModifier (..),
                                              GenesisStakeholders, MonadUtxoRead (..),
                                              ToilVerFailure (..), Utxo, evalUtxoStateT,
                                              runDBToil, runToilTLocalExtra, utxoGet)
import           Pos.Util.Chrono             (NewestFirst (..))
import qualified Pos.Util.Modifier           as MM

import           Pos.Explorer.Core           (TxExtra (..))
import           Pos.Explorer.Txp.Toil       (ExplorerExtra, ExplorerExtraTxp (..),
                                              MonadTxExtraRead (..), eNormalizeToil,
                                              eProcessTx, eeLocalTxsExtra)

type ETxpLocalWorkMode ctx m =
    ( MonadIO m
    , MonadBaseControl IO m
    , MonadDBRead m
    , MonadGState m
    , MonadTxpMem ExplorerExtra ctx m
    , WithLogger m
    , MonadSlots m
    , HasLens GenesisStakeholders ctx GenesisStakeholders
    )

type ETxpLocalDataPure = GenericTxpLocalDataPure ExplorerExtra

-- | A monad transformer whose purpose is to avoid overlapping instances
-- of MonadTxExtraRead (ReaderT ExplorerExtraTxp m).
newtype ExplorerReaderWrapper m a = ExplorerReaderWrapper
    { runExplorerReaderWrapper :: m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadError e
               , MonadUtxoRead
               , MonadGState
               )

instance Monad m => MonadTxExtraRead (ExplorerReaderWrapper (ReaderT ExplorerExtraTxp m)) where
    getTxExtra txId = HM.lookup txId . eetTxExtra <$> ExplorerReaderWrapper ask
    getAddrHistory addr = HM.lookupDefault (NewestFirst []) addr . eetAddrHistories <$> ExplorerReaderWrapper ask
    getAddrBalance addr = HM.lookup addr . eetAddrBalances <$> ExplorerReaderWrapper ask

eTxProcessTransaction
    :: ETxpLocalWorkMode ctx m
    => (TxId, TxAux) -> ExceptT ToilVerFailure m ()
eTxProcessTransaction itw@(txId, TxAux {taTx = UnsafeTx {..}}) = do
    tipBefore <- GS.getTip
    localUM <- lift getUtxoModifier
    epoch <- siEpoch <$> (note ToilSlotUnknown =<< getCurrentSlot)
    genStks <- view (lensOf @GenesisStakeholders)
    bvd <- gsAdoptedBVData
    -- Note: snapshot isn't used here, because it's not necessary.  If
    -- tip changes after 'getTip' and before resolving all inputs, it's
    -- possible that invalid transaction will appear in
    -- mempool. However, in this case it will be removed by
    -- normalization before releasing lock on block application.
    (resolvedOuts, _) <- runDBToil $ runUM localUM $ mapM utxoGet _txInputs
    -- Resolved are unspent transaction outputs corresponding to input
    -- of given transaction.
    let resolved = M.fromList $
                   catMaybes $
                   toList $
                   NE.zipWith (liftM2 (,) . Just) _txInputs resolvedOuts
    curTime <- currentTimeSlotting
    let txInAddrs = map (txOutAddress . toaOut) $ catMaybes $ toList resolvedOuts
        txOutAddrs = toList $ map txOutAddress _txOutputs
        allAddrs = ordNub $ txInAddrs <> txOutAddrs
    hmHistories <- buildMap allAddrs <$> mapM (fmap Just . ExDB.getAddrHistory) allAddrs
    hmBalances <- buildMap allAddrs <$> mapM ExDB.getAddrBalance allAddrs
    -- `eet` is passed to `processTxDo` where it is used in a ReaderT environment
    -- to provide underlying functions (`modifyAddrHistory` and `modifyAddrBalance`)
    -- with data to update. In case of `TxExtra` data is only added, but never updated,
    -- hence `mempty` here.
    let eet = ExplorerExtraTxp mempty hmHistories hmBalances
    pRes <- lift $ modifyTxpLocalData "eTxProcessTransaction" $
            processTxDo epoch bvd genStks resolved tipBefore itw curTime eet
    case pRes of
        Left er -> do
            logDebug $ sformat ("Transaction processing failed: "%build) txId
            throwError er
        Right _   ->
            logDebug $ sformat ("Transaction is processed successfully: "%build) txId
  where
    processTxDo
        :: EpochIndex
        -> BlockVersionData
        -> GenesisStakeholders
        -> Utxo
        -> HeaderHash
        -> (TxId, TxAux)
        -> Timestamp
        -> ExplorerExtraTxp
        -> ETxpLocalDataPure
        -> (Either ToilVerFailure (), ETxpLocalDataPure)
    processTxDo curEpoch bvd genStks resolved tipBefore tx curTime eet txld@(uv, mp, undo, tip, extra)
        | tipBefore /= tip = (Left $ ToilTipsMismatch tipBefore tip, txld)
        | otherwise =
            let execToil action =
                    snd <$> runToilTLocalExtra uv mp undo extra action
                -- NE.fromList is safe here, because if `resolved` is empty, `processTx`
                -- wouldn't save extra value, thus wouldn't reduce it to NF
                txUndo = NE.fromList $ toList resolved
                res =
                    (runExceptT $
                     flip evalUtxoStateT resolved $
                     flip runReaderT eet $
                     runExplorerReaderWrapper $
                     flip runReaderT genStks $
                     execToil $
                     eProcessTx curEpoch tx (TxExtra Nothing curTime txUndo)) bvd
            in case res of
                Left er  -> (Left er, txld)
                Right ToilModifier{..} ->
                    (Right (), (_tmUtxo, _tmMemPool, _tmUndos, tip, _tmExtra))
    runUM um = runToilTLocalExtra um def mempty (def @ExplorerExtra)
    buildMap :: (Eq a, Hashable a) => [a] -> [Maybe b] -> HM.HashMap a b
    buildMap keys maybeValues =
        HM.fromList $ catMaybes $ toList $
            zipWith (liftM2 (,) . Just) keys maybeValues

-- | 1. Recompute UtxoView by current MemPool
--   2. Remove invalid transactions from MemPool
--   3. Set new tip to txp local data
eTxNormalize ::
       ( ETxpLocalWorkMode ctx m
       , MonadSlots m
       )
    => m ()
eTxNormalize = do
    utxoTip <- GS.getTip
    epoch <- maybe (throwM ToilSlotUnknown) (pure . siEpoch) =<< getCurrentSlot
    localTxs <- getLocalTxsMap
    extra <- getTxpExtra
    let extras = MM.insertionsMap $ extra ^. eeLocalTxsExtra
    let toNormalize = HM.toList $ HM.intersectionWith (,) localTxs extras
    ToilModifier {..} <-
        runDBToil $
        snd <$>
        runToilTLocalExtra mempty def mempty def (eNormalizeToil epoch toNormalize)
    setTxpLocalData "eTxNormalize" (_tmUtxo, _tmMemPool, _tmUndos, utxoTip, _tmExtra)
