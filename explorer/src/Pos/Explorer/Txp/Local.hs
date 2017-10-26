{-# LANGUAGE TypeFamilies #-}

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
import           Formatting             (build, sformat, (%))
import           System.Wlog            (NamedPureLogger, logDebug)

import           Pos.Core               (BlockVersionData, EpochIndex, HeaderHash,
                                         Timestamp, siEpoch)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.DB.Class           (MonadDBRead, MonadGState (..))
import qualified Pos.Explorer.DB        as ExDB
import qualified Pos.GState             as GS
import           Pos.Reporting          (reportError)
import           Pos.Slotting           (MonadSlots (getCurrentSlot), getSlotStart)
import           Pos.StateLock          (Priority (..), StateLock, StateLockMetrics,
                                         withStateLock)
import           Pos.Txp.Core           (Tx (..), TxAux (..), TxId, toaOut, txOutAddress)
import           Pos.Txp.Logic.Local    (ProcessTxContext (..), buildProccessTxContext)
import           Pos.Txp.MemState       (GenericTxpLocalDataPure, MempoolExt, MonadTxpMem,
                                         TxpLocalWorkMode, getLocalTxsMap, getTxpExtra,
                                         modifyTxpLocalData, setTxpLocalData)
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
    ( TxpLocalWorkMode ctx m
    , MempoolExt m ~ ExplorerExtra
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
type EProcessTxMode = ReaderT EProcessTxContext (NamedPureLogger Identity)

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
    => (TxId, TxAux) -> m (Either ToilVerFailure ())
eTxProcessTransaction itw =
    withStateLock LowPriority "eTxProcessTransaction" $ \__tip -> eTxProcessTransactionNoLock itw

eTxProcessTransactionNoLock
    :: ETxpLocalWorkMode ctx m
    => (TxId, TxAux) -> m (Either ToilVerFailure ())
eTxProcessTransactionNoLock itw@(txId, txAux) = reportTipMismatch $ runExceptT $ do
    tipBefore <- GS.getTip
    slot         <- note ToilSlotUnknown =<< getCurrentSlot
    -- First get the current @SlotId@ so we can calculate the time.
    -- Then get when that @SlotId@ started and use that as a time for @Tx@.
    let epoch     = siEpoch slot
    mTxTimestamp <- getSlotStart slot
    ctx <- lift $ buildEProcessTxContext txAux
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
        -> NamedPureLogger Identity (Either ToilVerFailure (), ETxpLocalDataPure)
    processTxDo curEpoch ctx@EProcessTxContext {..} tipBefore tx mTxTimestamp txld@(uv, mp, undo, tip, extra)
        | tipBefore /= tip = pure (Left $ ToilTipsMismatch tipBefore tip, txld)
        | otherwise = do
            let runToil ::
                       Functor m
                    => ToilT ExplorerExtra m a
                    -> m (a, GenericToilModifier ExplorerExtra)
                runToil = runToilTLocalExtra uv mp undo extra

                -- NE.fromList is safe here, because if `resolved` is empty, `processTx`
                -- wouldn't save extra value, thus wouldn't reduce it to NF
                txUndo = NE.fromList $ map Just $ toList _eptcUtxoBase

                -- We strictly rely on verifyAllIsKnown = True here
                action ::
                       ExceptT ToilVerFailure (ToilT ExplorerExtra EProcessTxMode) ()
                action = eProcessTx curEpoch tx (TxExtra Nothing mTxTimestamp txUndo)
            res :: ( Either ToilVerFailure (), GenericToilModifier ExplorerExtra) <-
                    flip runReaderT ctx $
                    runToil $ runExceptT action
            case res of
                (Left er, _) -> pure (Left er, txld)
                (Right (), ToilModifier {..}) -> pure
                    ( Right ()
                    , (_tmUtxo, _tmMemPool, _tmUndos, tip, _tmExtra))
    -- REPORT:ERROR Tips mismatch in txp.
    reportTipMismatch action = do
        res <- action
        res <$ case res of
            (Left err@(ToilTipsMismatch {})) -> reportError (pretty err)
            _                                -> pass

buildEProcessTxContext
    :: forall m ctx.
       ( MonadIO m
       , MonadDBRead m
       , MonadGState m
       , MonadTxpMem (MempoolExt m) ctx m
       )
    => TxAux -> m EProcessTxContext
buildEProcessTxContext txAux = do
    ProcessTxContext{..} <- buildProccessTxContext txAux
    let UnsafeTx {..} = taTx txAux
    let txInAddrs = map (txOutAddress . toaOut) $ toList _ptcUtxoBase
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
    pure $
        EProcessTxContext
        { _eptcExtraBase = eet
        , _eptcAdoptedBVData = _ptcAdoptedBVData
        , _eptcUtxoBase = _ptcUtxoBase
        }
  where
    buildMap :: (Eq a, Hashable a) => [a] -> [Maybe b] -> HM.HashMap a b
    buildMap keys maybeValues =
        HM.fromList $
        catMaybes $ toList $ zipWith (liftM2 (,) . Just) keys maybeValues

-- | 1. Recompute UtxoView by current MemPool
--   2. Remove invalid transactions from MemPool
--   3. Set new tip to txp local data
eTxNormalize ::
       ( ETxpLocalWorkMode ctx m
       , MonadSlots ctx m
       )
    => m ()
eTxNormalize = getCurrentSlot >>= \case
    Nothing -> do
        tip <- GS.getTip
        -- Clear and update tip
        setTxpLocalData (mempty, def, mempty, tip, def)
    Just (siEpoch -> epoch) -> do
        utxoTip <- GS.getTip
        localTxs <- getLocalTxsMap
        extra <- getTxpExtra
        let extras = MM.insertionsMap $ extra ^. eeLocalTxsExtra
        let toNormalize = HM.toList $ HM.intersectionWith (,) localTxs extras
        ToilModifier {..} <-
            runDBToil $
            snd <$>
            runToilTLocalExtra mempty def mempty def (eNormalizeToil epoch toNormalize)
        setTxpLocalData (_tmUtxo, _tmMemPool, _tmUndos, utxoTip, _tmExtra)
