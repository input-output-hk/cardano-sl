-- | Explorer's local Txp.

module Pos.Explorer.Txp.Local
       ( eTxProcessTransaction
       , eTxNormalize
       ) where

import           Universum

import           Control.Monad.Except  (MonadError (..))
import           Data.Default          (def)
import qualified Data.HashMap.Strict   as HM
import qualified Data.List.NonEmpty    as NE
import qualified Data.Map              as M (fromList)
import           Formatting            (build, sformat, (%))
import           System.Wlog           (WithLogger, logDebug)

import           Pos.Core              (HeaderHash, Timestamp)
import           Pos.DB.Class          (MonadDB, MonadDBPure)
import qualified Pos.DB.GState         as GS
import qualified Pos.Explorer.DB       as ExDB
import           Pos.Slotting          (MonadSlots (currentTimeSlotting))
import           Pos.Txp.Core          (Tx (..), TxAux (..), TxId, toaOut, txOutAddress)
import           Pos.Txp.MemState      (GenericTxpLocalDataPure, MonadTxpMem,
                                        getLocalTxsMap, getTxpExtra, getUtxoModifier,
                                        modifyTxpLocalData, setTxpLocalData)
import           Pos.Txp.Toil          (GenericToilModifier (..), MonadToilEnv,
                                        MonadUtxoRead (..), ToilEnv, ToilVerFailure (..),
                                        Utxo, getToilEnv, runDBTxp, runToilTLocalExtra,
                                        runUtxoReaderT, utxoGet)
import           Pos.Util.Chrono       (NewestFirst (..))
import qualified Pos.Util.Modifier     as MM

import           Pos.Explorer.Core     (TxExtra (..))
import           Pos.Explorer.Txp.Toil (ExplorerExtra, ExplorerExtraTxp (..),
                                        MonadTxExtraRead (..), eNormalizeToil, eProcessTx,
                                        eeLocalTxsExtra)

type ETxpLocalWorkMode m =
    ( MonadDB m
    , MonadDBPure m
    , MonadTxpMem ExplorerExtra m
    , WithLogger m
    , MonadError ToilVerFailure m
    , MonadSlots m
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
               , MonadToilEnv
               )

instance Monad m => MonadTxExtraRead (ExplorerReaderWrapper (ReaderT ExplorerExtraTxp m)) where
    getTxExtra txId = HM.lookup txId . eetTxExtra <$> ExplorerReaderWrapper ask
    getAddrHistory addr = HM.lookupDefault (NewestFirst []) addr . eetAddrHistories <$> ExplorerReaderWrapper ask
    getAddrBalance addr = HM.lookup addr . eetAddrBalances <$> ExplorerReaderWrapper ask

eTxProcessTransaction
    :: ETxpLocalWorkMode m
    => (TxId, TxAux) -> m ()
eTxProcessTransaction itw@(txId, TxAux {taTx = UnsafeTx {..}}) = do
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
    pRes <- modifyTxpLocalData $
            processTxDo resolved toilEnv tipBefore itw curTime eet
    case pRes of
        Left er -> do
            logDebug $ sformat ("Transaction processing failed: "%build) txId
            throwError er
        Right _   ->
            logDebug $ sformat ("Transaction is processed successfully: "%build) txId
  where
    processTxDo
        :: Utxo
        -> ToilEnv
        -> HeaderHash
        -> (TxId, TxAux)
        -> Timestamp
        -> ExplorerExtraTxp
        -> ETxpLocalDataPure
        -> (Either ToilVerFailure (), ETxpLocalDataPure)
    processTxDo resolved toilEnv tipBefore tx curTime eet txld@(uv, mp, undo, tip, extra)
        | tipBefore /= tip = (Left $ ToilTipsMismatch tipBefore tip, txld)
        | otherwise =
            let execToil action =
                    snd <$> runToilTLocalExtra uv mp undo extra action
                -- NE.fromList is safe here, because if `resolved` is empty, `processTx`
                -- wouldn't save extra value, thus wouldn't reduce it to NF
                txUndo = NE.fromList $ toList resolved
                res =
                    (runExceptT $
                     flip runUtxoReaderT resolved $
                     flip runReaderT eet $
                     runExplorerReaderWrapper $
                     execToil $
                     eProcessTx tx (TxExtra Nothing curTime txUndo))
                        toilEnv
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
eTxNormalize
    :: (MonadDB m, MonadDBPure m, MonadTxpMem ExplorerExtra m) => m ()
eTxNormalize = do
    utxoTip <- GS.getTip
    localTxs <- getLocalTxsMap
    extra <- getTxpExtra
    let extras = MM.insertionsMap $ extra ^. eeLocalTxsExtra
    let toNormalize = HM.toList $ HM.intersectionWith (,) localTxs extras
    ToilModifier {..} <-
        runDBTxp $
        snd <$>
        runToilTLocalExtra mempty def mempty def (eNormalizeToil toNormalize)
    setTxpLocalData (_tmUtxo, _tmMemPool, _tmUndos, utxoTip, _tmExtra)
