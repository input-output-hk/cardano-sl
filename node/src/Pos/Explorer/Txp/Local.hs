-- | Explorer's local Txp.

module Pos.Explorer.Txp.Local
       ( eTxProcessTransaction
       , eTxProcessTransactionNoLock
       , eTxNormalize
       ) where

import           Universum

import           Control.Lens           (makeLenses)
import           Control.Monad.Except   (MonadError (..))
import           Data.Default           (def)
import qualified Data.HashMap.Strict    as HM
import qualified Data.List.NonEmpty     as NE
import qualified Data.Map               as M (fromList)
import           Formatting             (build, sformat, (%))
import           Mockable               (CurrentTime, Mockable)
import           System.Wlog            (WithLogger, logDebug)

import           Pos.Core               (BlockVersionData, EpochIndex, HeaderHash,
                                         Timestamp, siEpoch)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.DB.Class           (MonadDBRead, MonadGState (..))
import qualified Pos.Explorer.DB        as ExDB
import qualified Pos.GState             as GS
import           Pos.Reporting          (MonadReporting, reportError)
import           Pos.Slotting           (MonadSlots (getCurrentSlot), getSlotStart)
import           Pos.StateLock          (Priority (..), StateLock, StateLockMetrics,
                                         withStateLock)
import           Pos.Txp.Core           (Tx (..), TxAux (..), TxId, toaOut, txOutAddress)
import           Pos.Txp.MemState       (GenericTxpLocalDataPure, MemPoolSnapshot,
                                         MonadTxpMem, getLocalTxsMap, getTxpExtra,
                                         getUtxoModifier, modifyTxpLocalData,
                                         setTxpLocalData)
import           Pos.Txp.Toil           (GenericToilModifier (..), MonadUtxoRead (..),
                                         ToilT, ToilVerFailure (..), Utxo, runDBToil,
                                         runDBToil, runToilTLocalExtra, utxoGet,
                                         utxoGetReader)
import           Pos.Util.Chrono        (NewestFirst (..))
import qualified Pos.Util.Modifier      as MM
import           Pos.Util.Util          (HasLens (..), HasLens')

import           Pos.Explorer.Core      (TxExtra (..))
import           Pos.Explorer.Txp.Toil  (ExplorerExtra, ExplorerExtraTxp (..),
                                         MonadTxExtraRead (..), eNormalizeToil,
                                         eProcessTx, eeLocalTxsExtra)


type ETxpLocalWorkMode ctx m =
    ( MonadIO m
    , MonadDBRead m
    , MonadGState m
    , MonadTxpMem ExplorerExtra ctx m
    , WithLogger m
    , MonadSlots ctx m
    , Mockable CurrentTime m
    , MonadMask m
    , MonadReporting ctx m
    )

type ETxpLocalDataPure = GenericTxpLocalDataPure ExplorerExtra

-- Base context for tx processing in explorer.
data EProcessTxContext = EProcessTxContext
    { _eptcExtraBase     :: !ExplorerExtraTxp
    , _eptcAdoptedBVData :: !BlockVersionData
    , _eptcUtxoBase      :: !Utxo
    }

makeLenses ''EProcessTxContext

instance HasLens Utxo EProcessTxContext Utxo where
    lensOf = eptcUtxoBase

-- Base monad for tx processing in explorer.
type EProcessTxMode = Reader EProcessTxContext

instance HasConfiguration => MonadUtxoRead EProcessTxMode where
    utxoGet = utxoGetReader

instance MonadGState EProcessTxMode where
    gsAdoptedBVData = view eptcAdoptedBVData

instance MonadTxExtraRead EProcessTxMode where
    getTxExtra txId = HM.lookup txId . eetTxExtra <$> view eptcExtraBase
    getAddrHistory addr =
        HM.lookupDefault (NewestFirst []) addr . eetAddrHistories <$>
        view eptcExtraBase
    getAddrBalance addr =
        HM.lookup addr . eetAddrBalances <$> view eptcExtraBase
    getUtxoSum =
        eetUtxoSum <$> view eptcExtraBase

eTxProcessTransaction
    :: (ETxpLocalWorkMode ctx m, MonadMask m,
        HasLens' ctx StateLock, HasLens' ctx StateLockMetrics)
    => MemPoolSnapshot -> (TxId, TxAux) -> m (Either ToilVerFailure ())
eTxProcessTransaction mps itw =
    withStateLock LowPriority "eTxProcessTransaction" $ \__tip -> eTxProcessTransactionNoLock mps itw

eTxProcessTransactionNoLock
    :: (ETxpLocalWorkMode ctx m)
    => MemPoolSnapshot -> (TxId, TxAux) -> m (Either ToilVerFailure ())
eTxProcessTransactionNoLock mps itw@(txId, txAux) = reportTipMismatch $ runExceptT $ do
    let UnsafeTx {..} = taTx txAux
    -- Note: we need to read tip from the DB and check that it's the
    -- same as the one in mempool. That's because mempool state is
    -- valid only with respect to the tip stored there. Normally tips
    -- will match, because whenever we apply/rollback blocks we
    -- normalize mempool. However, there is a corner case when we
    -- receive an unexpected exception after modifying GState and
    -- before normalization. In this case normalization can fail and
    -- tips will differ. Rejecting transactions in this case should be
    -- fine, because the fact that we receive exceptions likely
    -- indicates that something is bad and we have more serious issues.
    --
    -- Also note that we don't need to use a snapshot here and can be
    -- sure that GState won't change, because changing it requires
    -- 'StateLock' which we own inside this function.
    tipBefore <- GS.getTip
    bvd <- gsAdoptedBVData
    let localUM = getUtxoModifier mps
    (resolvedOuts, _) <- runDBToil $ runUM localUM $ mapM utxoGet _txInputs
    -- Resolved are unspent transaction outputs corresponding to input
    -- of given transaction.
    let resolved =
            M.fromList $
            catMaybes $
            toList $ NE.zipWith (liftM2 (,) . Just) _txInputs resolvedOuts
    -- First get the current @SlotId@ so we can calculate the time.
    -- Then get when that @SlotId@ started and use that as a time for @Tx@.
    slot         <- note ToilSlotUnknown =<< getCurrentSlot
    let epoch     = siEpoch slot
    mTxTimestamp <- getSlotStart slot

    let txInAddrs =
            map (txOutAddress . toaOut) $ catMaybes $ toList resolvedOuts
        txOutAddrs = toList $ map txOutAddress _txOutputs
        allAddrs = ordNub $ txInAddrs <> txOutAddrs
    hmHistories <-
        buildMap allAddrs <$> mapM (fmap Just . ExDB.getAddrHistory) allAddrs
    hmBalances <- buildMap allAddrs <$> mapM ExDB.getAddrBalance allAddrs
    utxoSum <- ExDB.getUtxoSum
    -- `eet` is passed to `processTxDo` where it is used in a ReaderT environment
    -- to provide underlying functions (`modifyAddrHistory` and `modifyAddrBalance`)
    -- with data to update. In case of `TxExtra` data is only added, but never updated,
    -- hence `mempty` here.
    let eet = ExplorerExtraTxp mempty hmHistories hmBalances utxoSum
    let ctx =
            EProcessTxContext
            { _eptcExtraBase = eet
            , _eptcAdoptedBVData = bvd
            , _eptcUtxoBase = resolved
            }
    pRes <-
        lift $
        modifyTxpLocalData $
        processTxDo epoch ctx tipBefore itw mTxTimestamp
    -- We report 'ToilTipsMismatch' as an error, because usually it
    -- should't happen. If it happens, it's better to look at logs.
    case pRes of
        Left er -> do
            logDebug $ sformat ("Transaction processing failed: " %build) txId
            throwError er
        Right _ ->
            logDebug
                (sformat ("Transaction is processed successfully: " %build) txId)
  where
    processTxDo ::
           EpochIndex
        -> EProcessTxContext
        -> HeaderHash
        -> (TxId, TxAux)
        -> Maybe Timestamp
        -> ETxpLocalDataPure
        -> (Either ToilVerFailure (), ETxpLocalDataPure)
    processTxDo curEpoch ctx@EProcessTxContext {..} tipBefore tx mTxTimestamp txld@(uv, mp, undo, tip, extra)
        | tipBefore /= tip = (Left $ ToilTipsMismatch tipBefore tip, txld)
        | otherwise =
            let runToil ::
                       Functor m
                    => ToilT ExplorerExtra m a
                    -> m (a, GenericToilModifier ExplorerExtra)
                runToil = runToilTLocalExtra uv mp undo extra
                -- We strictly rely on verifyAllIsKnown = True here
                action ::
                       ExceptT ToilVerFailure (ToilT ExplorerExtra EProcessTxMode) ()
                action = eProcessTx curEpoch tx (TxExtra Nothing mTxTimestamp txUndo)
                -- NE.fromList is safe here, because if `resolved` is empty, `processTx`
                -- wouldn't save extra value, thus wouldn't reduce it to NF
                txUndo = NE.fromList $ map Just $ toList _eptcUtxoBase
                res :: ( Either ToilVerFailure ()
                       , GenericToilModifier ExplorerExtra)
                res = usingReader ctx $ runToil $ runExceptT action
            in case res of
                   (Left er, _) -> (Left er, txld)
                   (Right (), ToilModifier {..}) ->
                       ( Right ()
                       , (_tmUtxo, _tmMemPool, _tmUndos, tip, _tmExtra))
    runUM um = runToilTLocalExtra um def mempty (def @ExplorerExtra)
    buildMap :: (Eq a, Hashable a) => [a] -> [Maybe b] -> HM.HashMap a b
    buildMap keys maybeValues =
        HM.fromList $
        catMaybes $ toList $ zipWith (liftM2 (,) . Just) keys maybeValues
    -- REPORT:ERROR Tips mismatch in txp.
    reportTipMismatch action = do
        res <- action
        res <$ case res of
            (Left err@(ToilTipsMismatch {})) -> reportError (pretty err)
            _                                -> pass

-- | 1. Recompute UtxoView by current MemPool
--   2. Remove invalid transactions from MemPool
--   3. Set new tip to txp local data
eTxNormalize ::
       ( ETxpLocalWorkMode ctx m
       , MonadSlots ctx m
       )
    => MemPoolSnapshot -> m ()
eTxNormalize memPoolSnapshot = getCurrentSlot >>= \case
    Nothing -> do
        tip <- GS.getTip
        -- Clear and update tip
        setTxpLocalData (mempty, def, mempty, tip, def)
    Just (siEpoch -> epoch) -> do
        utxoTip <- GS.getTip
        let localTxs = getLocalTxsMap memPoolSnapshot
        extra <- getTxpExtra
        let extras = MM.insertionsMap $ extra ^. eeLocalTxsExtra
        let toNormalize = HM.toList $ HM.intersectionWith (,) localTxs extras
        ToilModifier {..} <-
            runDBToil $
            snd <$>
            runToilTLocalExtra mempty def mempty def (eNormalizeToil epoch toNormalize)
        setTxpLocalData (_tmUtxo, _tmMemPool, _tmUndos, utxoTip, _tmExtra)
