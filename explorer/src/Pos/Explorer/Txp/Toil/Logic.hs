{-# LANGUAGE TypeFamilies #-}

-- | Explorer's version of Toil logic.

module Pos.Explorer.Txp.Toil.Logic
       ( EGlobalApplyToilMode
       , EGlobalVerifyToilMode
       , eApplyToil
       , eRollbackToil
       , eNormalizeToil
       , eProcessTx
       ) where

import           Universum

import           Control.Monad.Except (MonadError (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.List (delete)
import qualified Data.List.NonEmpty as NE
import           Formatting (build, sformat, (%))
import           System.Wlog (WithLogger, logError)

import           Pos.Core (Address, Coin, EpochIndex, HeaderHash, Timestamp, mkCoin, sumCoins,
                           unsafeAddCoin, unsafeSubCoin)
import           Pos.Core.Txp (Tx (..), TxAux (..), TxId, TxOut (..), TxOutAux (..), TxUndo, _TxOut)
import           Pos.Crypto (WithHash (..), hash)
import           Pos.Explorer.Core (AddrHistory, TxExtra (..))
import           Pos.Explorer.Txp.Toil.Class (MonadTxExtra (..), MonadTxExtraRead (..))
import           Pos.Txp.Toil (ToilVerFailure (..))
import qualified Pos.Txp.Toil as Txp
import           Pos.Txp.Topsort (topsortTxs)
import           Pos.Util.Chrono (NewestFirst (..))
import           Pos.Util.Util (Sign (..))

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

type EGlobalApplyToilMode m =
    ( Txp.GlobalApplyToilMode m
    , MonadTxExtra m
    )

type EGlobalVerifyToilMode m =
    ( Txp.GlobalVerifyToilMode m
    , MonadTxExtra m
    )

-- | Apply transactions from one block. They must be valid (for
-- example, it implies topological sort).
eApplyToil
    :: EGlobalApplyToilMode m
    => Maybe Timestamp
    -> [(TxAux, TxUndo)]
    -> HeaderHash
    -> m ()
eApplyToil mTxTimestamp txun hh = do
    Txp.applyToil txun
    mapM_ applier $ zip [0..] txun
  where
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
eRollbackToil :: EGlobalApplyToilMode m => [(TxAux, TxUndo)] -> m ()
eRollbackToil txun = do
    Txp.rollbackToil txun
    mapM_ extraRollback $ reverse txun
  where
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

type ELocalToilMode m =
    ( Txp.LocalToilMode m
    , MonadTxExtra m
    )

-- | Verify one transaction and also add it to mem pool and apply to utxo
-- if transaction is valid.
eProcessTx
    :: (ELocalToilMode m, MonadError ToilVerFailure m)
    => EpochIndex -> (TxId, TxAux) -> (TxUndo -> TxExtra) -> m ()
eProcessTx curEpoch tx@(id, aux) createExtra = do
    undo <- Txp.processTx curEpoch tx
    let extra = createExtra undo
    putTxExtraWithHistory id extra $ getTxRelatedAddrs aux undo
    let balanceUpdate = getBalanceUpdate aux undo
    updateAddrBalances balanceUpdate
    updateUtxoSumFromBalanceUpdate balanceUpdate

-- | Get rid of invalid transactions.
-- All valid transactions will be added to mem pool and applied to utxo.
eNormalizeToil
    :: ELocalToilMode m
    => EpochIndex
    -> [(TxId, (TxAux, TxExtra))]
    -> m ()
eNormalizeToil curEpoch txs = mapM_ normalize ordered
  where
    ordered = fromMaybe txs $ topsortTxs wHash txs
    wHash (i, (txAux, _)) = WithHash (taTx txAux) i
    normalize = runExceptT . uncurry (eProcessTx curEpoch) . repair
    repair (i, (txAux, extra)) = ((i, txAux), const extra)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

data BalanceUpdate = BalanceUpdate
    { minusBalance :: [(Address, Coin)]
    , plusBalance  :: [(Address, Coin)]
    }

modifyAddrHistory
    :: MonadTxExtra m
    => (AddrHistory -> AddrHistory)
    -> Address
    -> m ()
modifyAddrHistory f addr =
    updateAddrHistory addr . f =<< getAddrHistory addr

putTxExtraWithHistory
    :: MonadTxExtra m
    => TxId
    -> TxExtra
    -> NonEmpty Address
    -> m ()
putTxExtraWithHistory id extra addrs = do
    putTxExtra id extra
    for_ addrs $ modifyAddrHistory $
        NewestFirst . (id :) . getNewestFirst

delTxExtraWithHistory
    :: MonadTxExtra m
    => TxId
    -> NonEmpty Address
    -> m ()
delTxExtraWithHistory id addrs = do
    delTxExtra id
    for_ addrs $ modifyAddrHistory $
        NewestFirst . delete id . getNewestFirst

updateUtxoSumFromBalanceUpdate :: MonadTxExtra m => BalanceUpdate -> m ()
updateUtxoSumFromBalanceUpdate balanceUpdate = do
    let plusChange  = sumCoins $ map snd $ plusBalance  balanceUpdate
        minusChange = sumCoins $ map snd $ minusBalance balanceUpdate
        utxoChange  = plusChange - minusChange
    utxoSum <- getUtxoSum
    putUtxoSum $ utxoSum + utxoChange

getTxRelatedAddrs :: TxAux -> TxUndo -> NonEmpty Address
getTxRelatedAddrs TxAux {taTx = UncheckedTx {..}} (catMaybes . toList -> undo) =
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

updateAddrBalances :: (MonadTxExtra m, WithLogger m) => BalanceUpdate -> m ()
updateAddrBalances (combineBalanceUpdates -> updates) = mapM_ updater updates
  where
    updater :: (MonadTxExtra m, WithLogger m) => (Address, (Sign, Coin)) -> m ()
    updater (addr, (Plus, coin)) = do
        currentBalance <- fromMaybe (mkCoin 0) <$> getAddrBalance addr
        let newBalance = unsafeAddCoin currentBalance coin
        putAddrBalance addr newBalance
    updater (addr, (Minus, coin)) = do
        maybeBalance <- getAddrBalance addr
        case maybeBalance of
            Nothing ->
                logError $
                    sformat ("updateAddrBalances: attempted to subtract "%build%" from unknown address "%build)
                    coin addr
            Just currentBalance
                | currentBalance < coin ->
                    logError $
                        sformat ("updateAddrBalances: attempted to subtract "%build%" from address "%build%" which only has "%build)
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
