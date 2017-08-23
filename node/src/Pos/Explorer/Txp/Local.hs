-- | Explorer's local Txp.

module Pos.Explorer.Txp.Local
       ( eTxProcessTransaction
       , eTxNormalize
       ) where

import           Universum

import           Control.Lens                (makeLenses)
import           Control.Monad.Except        (MonadError (..))
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Default                (def)
import qualified Data.HashMap.Strict         as HM
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map                    as M (fromList)
import           Ether.Internal              (HasLens (..))
import           Formatting                  (build, sformat, (%))
import           System.Wlog                 (WithLogger, logDebug)

import           Pos.Core                    (BlockVersionData, EpochIndex,
                                              GenesisWStakeholders, HeaderHash, Timestamp,
                                              siEpoch)
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
                                              MonadUtxoRead (..), ToilT,
                                              ToilVerFailure (..), Utxo, runDBToil,
                                              runDBToil, runToilTLocalExtra, utxoGet,
                                              utxoGetReader)
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
    , MonadSlots ctx m
    , HasLens GenesisWStakeholders ctx GenesisWStakeholders
    )

type ETxpLocalDataPure = GenericTxpLocalDataPure ExplorerExtra

-- Base context for tx processing in explorer.
data EProcessTxContext = EProcessTxContext
    { _eptcExtraBase       :: !ExplorerExtraTxp
    , _eptcGenStakeholders :: !GenesisWStakeholders
    , _eptcAdoptedBVData   :: !BlockVersionData
    , _eptcUtxoBase        :: !Utxo
    }

makeLenses ''EProcessTxContext

instance HasLens GenesisWStakeholders EProcessTxContext GenesisWStakeholders where
    lensOf = eptcGenStakeholders

instance HasLens Utxo EProcessTxContext Utxo where
    lensOf = eptcUtxoBase

-- Base monad for tx processing in explorer.
type EProcessTxMode = Reader EProcessTxContext

instance MonadUtxoRead EProcessTxMode where
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

eTxProcessTransaction
    :: ETxpLocalWorkMode ctx m
    => (TxId, TxAux) -> ExceptT ToilVerFailure m ()
eTxProcessTransaction itw@(txId, TxAux {taTx = UnsafeTx {..}}) = do
    tipBefore <- GS.getTip
    localUM <- lift getUtxoModifier
    epoch <- siEpoch <$> (note ToilSlotUnknown =<< getCurrentSlot)
    genStks <- view (lensOf @GenesisWStakeholders)
    bvd <- gsAdoptedBVData
    -- Note: snapshot isn't used here, because it's not necessary.  If
    -- tip changes after 'getTip' and before resolving all inputs, it's
    -- possible that invalid transaction will appear in
    -- mempool. However, in this case it will be removed by
    -- normalization before releasing lock on block application.
    (resolvedOuts, _) <- runDBToil $ runUM localUM $ mapM utxoGet _txInputs
    -- Resolved are unspent transaction outputs corresponding to input
    -- of given transaction.
    let resolved =
            M.fromList $
            catMaybes $
            toList $ NE.zipWith (liftM2 (,) . Just) _txInputs resolvedOuts
    curTime <- currentTimeSlotting
    let txInAddrs =
            map (txOutAddress . toaOut) $ catMaybes $ toList resolvedOuts
        txOutAddrs = toList $ map txOutAddress _txOutputs
        allAddrs = ordNub $ txInAddrs <> txOutAddrs
    hmHistories <-
        buildMap allAddrs <$> mapM (fmap Just . ExDB.getAddrHistory) allAddrs
    hmBalances <- buildMap allAddrs <$> mapM ExDB.getAddrBalance allAddrs
    -- `eet` is passed to `processTxDo` where it is used in a ReaderT environment
    -- to provide underlying functions (`modifyAddrHistory` and `modifyAddrBalance`)
    -- with data to update. In case of `TxExtra` data is only added, but never updated,
    -- hence `mempty` here.
    let eet = ExplorerExtraTxp mempty hmHistories hmBalances
    let ctx =
            EProcessTxContext
            { _eptcExtraBase = eet
            , _eptcAdoptedBVData = bvd
            , _eptcUtxoBase = resolved
            , _eptcGenStakeholders = genStks
            }
    pRes <-
        lift $
        modifyTxpLocalData "eTxProcessTransaction" $
        processTxDo epoch ctx tipBefore itw curTime
    case pRes of
        Left er -> do
            logDebug $ sformat ("Transaction processing failed: " %build) txId
            throwError er
        Right _ ->
            logDebug $
            sformat ("Transaction is processed successfully: " %build) txId
  where
    processTxDo ::
           EpochIndex
        -> EProcessTxContext
        -> HeaderHash
        -> (TxId, TxAux)
        -> Timestamp
        -> ETxpLocalDataPure
        -> (Either ToilVerFailure (), ETxpLocalDataPure)
    processTxDo curEpoch ctx@EProcessTxContext {..} tipBefore tx curTime txld@(uv, mp, undo, tip, extra)
        | tipBefore /= tip = (Left $ ToilTipsMismatch tipBefore tip, txld)
        | otherwise =
            let runToil ::
                       Functor m
                    => ToilT ExplorerExtra m a
                    -> m (a, GenericToilModifier ExplorerExtra)
                runToil = runToilTLocalExtra uv mp undo extra
                action ::
                       ExceptT ToilVerFailure (ToilT ExplorerExtra EProcessTxMode) ()
                action = eProcessTx curEpoch tx (TxExtra Nothing curTime txUndo)
                -- NE.fromList is safe here, because if `resolved` is empty, `processTx`
                -- wouldn't save extra value, thus wouldn't reduce it to NF
                txUndo = NE.fromList $ toList _eptcUtxoBase
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
        setTxpLocalData "eTxNormalize" (mempty, def, mempty, tip, def)
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
        setTxpLocalData "eTxNormalize" (_tmUtxo, _tmMemPool, _tmUndos, utxoTip, _tmExtra)
