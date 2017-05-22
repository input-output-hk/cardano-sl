-- | Explorer's local Txp.

module Pos.Explorer.Txp.Local
       ( eTxProcessTransaction
       , eTxNormalize
       ) where

import           Universum

import           Control.Monad.Except             (MonadError (..))
import           Control.Monad.Trans.Ether.Tagged (TaggedTrans (..))
import           Control.Monad.Trans.Identity     (IdentityT (..))
import           Data.Coerce                      (coerce)
import           Data.Default                     (def)
import qualified Data.HashMap.Strict              as HM
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as M (fromList)
import           Formatting                       (build, sformat, (%))
import           System.Wlog                      (WithLogger, logDebug)

import           Pos.Core                         (HeaderHash, Timestamp)
import           Pos.DB.Class                     (MonadDB)
import qualified Pos.DB.GState                    as GS
import           Pos.Slotting                     (MonadSlots (currentTimeSlotting))
import           Pos.Txp.Core                     (Tx (..), TxAux, TxId)
import           Pos.Txp.MemState                 (GenericTxpLocalDataPure, MonadTxpMem,
                                                   getLocalTxsMap, getTxpExtra,
                                                   getUtxoModifier, modifyTxpLocalData,
                                                   setTxpLocalData, MemPoolModifyReason (..),
                                                   TransactionProvenance (..))
import           Pos.Txp.Toil                     (GenericToilModifier (..),
                                                   MonadUtxoRead (..), ToilEnv,
                                                   ToilVerFailure (..), Utxo, getToilEnv,
                                                   runDBTxp, runToilTLocalExtra,
                                                   runUtxoReaderT, utxoGet)
import           Pos.Util.Chrono                  (NewestFirst (..))
import qualified Pos.Util.Modifier                as MM

import           Pos.Explorer.Core                (TxExtra (..))
import           Pos.Explorer.Txp.Toil            (ExplorerExtra, MonadTxExtraRead (..),
                                                   eNormalizeToil, eProcessTx,
                                                   eeLocalTxsExtra)

type ETxpLocalWorkMode m =
    ( MonadDB m
    , MonadTxpMem ExplorerExtra m
    , WithLogger m
    , MonadError ToilVerFailure m
    , MonadSlots m
    )

type ETxpLocalDataPure = GenericTxpLocalDataPure ExplorerExtra

data NoExtraTag

-- A simple monad transformer, the only purpose of which is to provide
-- 'MonadTxExtraRead' instance corresponding to absence of any extra
-- data used by explorer.
type NoExtra = TaggedTrans NoExtraTag IdentityT

runNoExtra :: NoExtra m a -> m a
runNoExtra = coerce

instance Monad m => MonadTxExtraRead (NoExtra m) where
    getTxExtra _ = pure Nothing
    getAddrHistory _ = pure $ NewestFirst []

eTxProcessTransaction
    :: ETxpLocalWorkMode m
    => TransactionProvenance
    -> (TxId, TxAux)
    -> m ()
eTxProcessTransaction txProvenance itw@(txId, (UnsafeTx{..}, _, _)) = do
    tipBefore <- GS.getTip
    localUM <- getUtxoModifier
    -- Note: snapshot isn't used here, because it's not necessary.  If
    -- tip changes after 'getTip' and before resolving all inputs, it's
    -- possible that invalid transaction will appear in
    -- mempool. However, in this case it will be removed by
    -- normalization before releasing lock on block application.
    (resolvedOuts, _) <- runDBTxp $ runUM localUM $ mapM utxoGet _txInputs
    toilEnv <- runDBTxp getToilEnv
    -- Resolved are unspent transaction outputs corresponding to input
    -- of given transaction.
    let resolved = M.fromList $
                   catMaybes $
                   toList $
                   NE.zipWith (liftM2 (,) . Just) _txInputs resolvedOuts
    curTime <- currentTimeSlotting
    pRes <- modifyTxpLocalData (ProcessTransaction txProvenance) $
            processTxDo resolved toilEnv tipBefore itw curTime
    case pRes of
        Left er -> do
            logDebug $ sformat ("Transaction processing failed: "%build) txId
            throwError er
        Right _   ->
            logDebug (sformat ("Transaction is processed successfully: "%build) txId)
  where
    processTxDo
        :: Utxo
        -> ToilEnv
        -> HeaderHash
        -> (TxId, TxAux)
        -> Timestamp
        -> ETxpLocalDataPure
        -> (Either ToilVerFailure (), ETxpLocalDataPure)
    processTxDo resolved toilEnv tipBefore tx curTime txld@(uv, mp, undo, tip, extra)
        | tipBefore /= tip = (Left $ ToilTipsMismatch tipBefore tip, txld)
        | otherwise =
            let execToil action =
                    snd <$> runToilTLocalExtra uv mp undo extra action
                res =
                    (runExceptT $
                     flip runUtxoReaderT resolved $
                     runNoExtra $
                     execToil $ eProcessTx tx (makeExtra resolved curTime))
                        toilEnv
            in case res of
                Left er  -> (Left er, txld)
                Right ToilModifier{..} ->
                    (Right (), (_tmUtxo, _tmMemPool, _tmUndos, tip, _tmExtra))
    runUM um = runToilTLocalExtra um def mempty (def @ExplorerExtra)
    makeExtra :: Utxo -> Timestamp -> TxExtra
    -- NE.fromList is safe here, because if `resolved` is empty, `processTx`
    -- wouldn't save extra value, thus wouldn't reduce it to NF
    makeExtra resolved curTime = TxExtra Nothing curTime $ NE.fromList $ toList resolved

-- | 1. Recompute UtxoView by current MemPool
-- | 2. Remove invalid transactions from MemPool
-- | 3. Set new tip to txp local data
eTxNormalize
    :: (MonadDB m, MonadTxpMem ExplorerExtra m)
    => MemPoolModifyReason
    -> m ()
eTxNormalize reason = do
    utxoTip <- GS.getTip
    localTxs <- getLocalTxsMap
    extra <- getTxpExtra
    let extras = MM.insertionsMap $ extra ^. eeLocalTxsExtra
    let toNormalize = HM.toList $ HM.intersectionWith (,) localTxs extras
    ToilModifier {..} <-
        runDBTxp $
        snd <$>
        runToilTLocalExtra mempty def mempty def (eNormalizeToil toNormalize)
    setTxpLocalData reason (_tmUtxo, _tmMemPool, _tmUndos, utxoTip, _tmExtra)
