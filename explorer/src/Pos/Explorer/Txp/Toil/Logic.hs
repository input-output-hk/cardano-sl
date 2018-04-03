{-# LANGUAGE TypeFamilies #-}

-- | Explorer's version of Toil logic.

module Pos.Explorer.Txp.Toil.Logic
       ( eApplyToil
       , eRollbackToil
       , eNormalizeToil
       , eProcessTx
       ) where

import           Universum hiding (id)

import           Control.Monad.Except (mapExceptT)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.List (delete)
import qualified Data.List.NonEmpty as NE
import           Formatting (build, sformat, (%))
import           System.Wlog (logError)

import           Pos.Core (Address, BlockVersionData, Coin, EpochIndex, HasConfiguration,
                           HeaderHash, Timestamp, mkCoin, sumCoins, unsafeAddCoin, unsafeSubCoin)
import           Pos.Core.Txp (Tx (..), TxAux (..), TxId, TxOut (..), TxOutAux (..), TxUndo, _TxOut)
import           Pos.Crypto (WithHash (..), hash)
import           Pos.Explorer.Core (AddrHistory, TxExtra (..))
import           Pos.Explorer.Txp.Toil.Monad (EGlobalToilM, ELocalToilM, ExplorerExtraM,
                                              delAddrBalance, delTxExtra,
                                              explorerExtraMToEGlobalToilM,
                                              explorerExtraMToELocalToilM, getAddrBalance,
                                              getAddrHistory, getTxExtra, getUtxoSum,
                                              putAddrBalance, putTxExtra, putUtxoSum,
                                              updateAddrHistory)
import           Pos.Txp.Configuration (HasTxpConfiguration)
import           Pos.Txp.Toil (ToilVerFailure (..), extendGlobalToilM, extendLocalToilM)
import qualified Pos.Txp.Toil as Txp
import           Pos.Txp.Topsort (topsortTxs)
import           Pos.Util.Chrono (NewestFirst (..))
import           Pos.Util.Util (Sign (..))

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

-- | Apply transactions from one block. They must be valid (for
-- example, it implies topological sort).
eApplyToil ::
       HasConfiguration
    => Maybe Timestamp
    -> [(TxAux, TxUndo)]
    -> HeaderHash
    -> EGlobalToilM ()
eApplyToil mTxTimestamp txun hh = do
    extendGlobalToilM $ Txp.applyToil txun
    explorerExtraMToEGlobalToilM $ mapM_ applier $ zip [0..] txun
  where
    applier :: (Word32, (TxAux, TxUndo)) -> ExplorerExtraM ()
    applier (i, (txAux, txUndo)) = do
        let tx = taTx txAux
            id = hash tx
            newExtra = TxExtra (Just (hh, i)) mTxTimestamp txUndo
        extra <- fromMaybe newExtra <$> getTxExtra id
        putTxExtraWithHistory id extra $ getTxRelatedAddrs txAux txUndo
        let balanceUpdate = getBalanceUpdate txAux txUndo
        updateAddrBalances balanceUpdate
        updateUtxoSumFromBalanceUpdate balanceUpdate

-- | Rollback transactions from one block.
eRollbackToil :: HasConfiguration => [(TxAux, TxUndo)] -> EGlobalToilM ()
eRollbackToil txun = do
    extendGlobalToilM $ Txp.rollbackToil txun
    explorerExtraMToEGlobalToilM $ mapM_ extraRollback $ reverse txun
  where
    extraRollback :: (TxAux, TxUndo) -> ExplorerExtraM ()
    extraRollback (txAux, txUndo) = do
        delTxExtraWithHistory (hash (taTx txAux)) $
          getTxRelatedAddrs txAux txUndo
        let BalanceUpdate {..} = getBalanceUpdate txAux txUndo
        let balanceUpdate = BalanceUpdate {
            plusBalance = minusBalance,
            minusBalance = plusBalance
        }
        updateAddrBalances balanceUpdate
        updateUtxoSumFromBalanceUpdate balanceUpdate

----------------------------------------------------------------------------
-- Local
----------------------------------------------------------------------------

-- | Verify one transaction and also add it to mem pool and apply to utxo
-- if transaction is valid.
eProcessTx ::
       (HasTxpConfiguration, HasConfiguration)
    => BlockVersionData
    -> EpochIndex
    -> (TxId, TxAux)
    -> (TxUndo -> TxExtra)
    -> ExceptT ToilVerFailure ELocalToilM ()
eProcessTx bvd curEpoch tx@(id, aux) createExtra = do
    undo <- mapExceptT extendLocalToilM $ Txp.processTx bvd curEpoch tx
    lift $ explorerExtraMToELocalToilM $ do
        let extra = createExtra undo
        putTxExtraWithHistory id extra $ getTxRelatedAddrs aux undo
        let balanceUpdate = getBalanceUpdate aux undo
        updateAddrBalances balanceUpdate
        updateUtxoSumFromBalanceUpdate balanceUpdate

-- | Get rid of invalid transactions.
-- All valid transactions will be added to mem pool and applied to utxo.
eNormalizeToil ::
       (HasTxpConfiguration, HasConfiguration)
    => BlockVersionData
    -> EpochIndex
    -> [(TxId, (TxAux, TxExtra))]
    -> ELocalToilM ()
eNormalizeToil bvd curEpoch txs = mapM_ normalize ordered
  where
    ordered = fromMaybe txs $ topsortTxs wHash txs
    wHash (i, (txAux, _)) = WithHash (taTx txAux) i
    normalize = runExceptT . uncurry (eProcessTx bvd curEpoch) . repair
    repair (i, (txAux, extra)) = ((i, txAux), const extra)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

data BalanceUpdate = BalanceUpdate
    { minusBalance :: [(Address, Coin)]
    , plusBalance  :: [(Address, Coin)]
    }

modifyAddrHistory :: (AddrHistory -> AddrHistory) -> Address -> ExplorerExtraM ()
modifyAddrHistory f addr = updateAddrHistory addr . f =<< getAddrHistory addr

putTxExtraWithHistory :: TxId -> TxExtra -> NonEmpty Address -> ExplorerExtraM ()
putTxExtraWithHistory id extra addrs = do
    putTxExtra id extra
    for_ addrs $ modifyAddrHistory $
        NewestFirst . (id :) . getNewestFirst

delTxExtraWithHistory :: TxId -> NonEmpty Address -> ExplorerExtraM ()
delTxExtraWithHistory id addrs = do
    delTxExtra id
    for_ addrs $ modifyAddrHistory $
        NewestFirst . delete id . getNewestFirst

updateUtxoSumFromBalanceUpdate :: BalanceUpdate -> ExplorerExtraM ()
updateUtxoSumFromBalanceUpdate balanceUpdate = do
    let plusChange  = sumCoins $ map snd $ plusBalance  balanceUpdate
        minusChange = sumCoins $ map snd $ minusBalance balanceUpdate
        utxoChange  = plusChange - minusChange
    utxoSum <- getUtxoSum
    putUtxoSum $ utxoSum + utxoChange

getTxRelatedAddrs :: TxAux -> TxUndo -> NonEmpty Address
getTxRelatedAddrs TxAux {taTx = UnsafeTx {..}} (catMaybes . toList -> undo) =
    map txOutAddress _txOutputs `unionNEnList` map (txOutAddress . toaOut) undo
  where
    toSet = HS.fromList . toList
    -- Safe here, because union of non-empty and maybe empty sets can't be empty.
    unionNEnList :: NonEmpty Address -> [Address] -> NonEmpty Address
    unionNEnList lhs rhs = NE.fromList $ toList $ HS.union (toSet lhs) (HS.fromList rhs)

combineBalanceUpdates :: BalanceUpdate -> [(Address, (Sign, Coin))]
combineBalanceUpdates BalanceUpdate {..} =
    let plusCombined  = HM.fromListWith unsafeAddCoin plusBalance
        minusCombined = HM.fromListWith unsafeAddCoin minusBalance
        bothCombined = outerJoin plusCombined minusCombined
        result = HM.mapMaybe reducer bothCombined
    in HM.toList result
  where
    outerJoin
        :: (Eq k, Hashable k)
        => HM.HashMap k v1
        -> HM.HashMap k v2
        -> HM.HashMap k (Maybe v1, Maybe v2)
    outerJoin hm1 hm2 =
        -- need to do an extra map because unionWith does not support changing type
        let hm1' = HM.map (\x -> (Just x, Nothing)) hm1
            hm2' = HM.map (\x -> (Nothing, Just x)) hm2
        in HM.unionWith joiner hm1' hm2'
    joiner (Just plus, Nothing) (Nothing, Just minus) = (Just plus, Just minus)
    joiner _ _ = error "combineBalanceUpdates: HashMap.map is broken"
    reducer (Just plus, Just minus)
        | plus > minus = Just (Plus,  unsafeSubCoin plus minus)
        | plus < minus = Just (Minus, unsafeSubCoin minus plus)
    reducer (Nothing, Nothing) = error "combineBalanceUpdates: HashMap.unionWith is broken"
    reducer (Just plus, Nothing)  | plus /= mkCoin 0  = Just (Plus, plus)
    reducer (Nothing, Just minus) | minus /= mkCoin 0 = Just (Minus, minus)
    reducer _ = Nothing

updateAddrBalances :: BalanceUpdate -> ExplorerExtraM ()
updateAddrBalances (combineBalanceUpdates -> updates) = mapM_ updater updates
  where
    updater :: (Address, (Sign, Coin)) -> ExplorerExtraM ()
    updater (addr, (Plus, coin)) = do
        currentBalance <- fromMaybe (mkCoin 0) <$> getAddrBalance addr
        let newBalance = unsafeAddCoin currentBalance coin
        putAddrBalance addr newBalance
    updater (addr, (Minus, coin)) = do
        maybeBalance <- getAddrBalance addr
        case maybeBalance of
            Nothing ->
                logError $
                    sformat ("updateAddrBalances: attempted to subtract "%build%
                             " from unknown address "%build)
                    coin addr
            Just currentBalance
                | currentBalance < coin ->
                    logError $
                        sformat ("updateAddrBalances: attempted to subtract "%build%
                                 " from address "%build%" which only has "%build)
                        coin addr currentBalance
                | otherwise -> do
                    let newBalance = unsafeSubCoin currentBalance coin
                    if newBalance == mkCoin 0 then
                        delAddrBalance addr
                    else
                        putAddrBalance addr newBalance

getBalanceUpdate :: TxAux -> TxUndo -> BalanceUpdate
getBalanceUpdate txAux txUndo =
    let minusBalance = map (view _TxOut . toaOut) $ catMaybes $ toList txUndo
        plusBalance = map (view _TxOut) $ toList $ _txOutputs (taTx txAux)
    in BalanceUpdate {..}
