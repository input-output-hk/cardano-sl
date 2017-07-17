{-# LANGUAGE TypeFamilies #-}

-- | Explorer's version of Toil logic.

module Pos.Explorer.Txp.Toil.Logic
       ( EGlobalToilMode
       , eApplyToil
       , eRollbackToil
       , eNormalizeToil
       , eProcessTx
       ) where

import           Universum

import           Control.Monad.Except        (MonadError (..))
import qualified Data.HashMap.Strict         as HM
import qualified Data.HashSet                as HS
import           Data.List                   (delete)
import qualified Data.List.NonEmpty          as NE
import           Formatting                  (build, sformat, (%))
import           System.Wlog                 (WithLogger, logError, runNamedPureLog,
                                              usingLoggerName)

import           Pos.Core                    (Address, Coin, HeaderHash, Timestamp,
                                              mkCoin, unsafeAddCoin, unsafeSubCoin)
import           Pos.Crypto                  (WithHash (..), hash)
import           Pos.Explorer.Core           (AddrHistory, TxExtra (..))
import           Pos.Explorer.Txp.Toil.Class (MonadTxExtra (..), MonadTxExtraRead (..))
import           Pos.Txp.Core                (Tx (..), TxAux (..), TxId, TxOut (..),
                                              TxOutAux (..), TxUndo, topsortTxs, _TxOut)
import           Pos.Txp.Toil                (ToilVerFailure (..))
import qualified Pos.Txp.Toil                as Txp
import           Pos.Util.Chrono             (NewestFirst (..))

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

type EGlobalToilMode m = ( Txp.GlobalToilMode m
                         , MonadTxExtra m
                         )

-- | Apply transactions from one block. They must be valid (for
-- example, it implies topological sort).
eApplyToil
    :: EGlobalToilMode m
    => Timestamp
    -> [(TxAux, TxUndo)]
    -> HeaderHash
    -> m ()
eApplyToil curTime txun hh = do
    Txp.applyToil txun
    mapM_ applier $ zip [0..] txun
  where
    applier (i, (txAux, txundo)) = do
        let tx = taTx txAux
            id = hash tx
            newExtra = TxExtra (Just (hh, i)) curTime txundo
        extra <- fromMaybe newExtra <$> getTxExtra id
        putTxExtraWithHistory id extra $ getTxRelatedAddrs txAux txundo
        let balanceUpdate = getBalanceUpdate txAux txundo
        updateAddrBalances balanceUpdate

-- | Rollback transactions from one block.
eRollbackToil :: EGlobalToilMode m => [(TxAux, TxUndo)] -> m ()
eRollbackToil txun = do
    Txp.rollbackToil txun
    mapM_ extraRollback txun
  where
    extraRollback (txAux, txundo) = do
        delTxExtraWithHistory (hash (taTx txAux)) $
          getTxRelatedAddrs txAux txundo
        let BalanceUpdate {..} = getBalanceUpdate txAux txundo
        updateAddrBalances BalanceUpdate {
            plusBalance = minusBalance,
            minusBalance = plusBalance
        }

----------------------------------------------------------------------------
-- Local
----------------------------------------------------------------------------

type ELocalToilMode m = ( Txp.LocalToilMode m
                        , MonadTxExtra m
                        )

-- | Verify one transaction and also add it to mem pool and apply to utxo
-- if transaction is valid.
eProcessTx
    :: (ELocalToilMode m, MonadError ToilVerFailure m)
    => (TxId, TxAux) -> TxExtra -> m ()
eProcessTx tx@(id, aux) extra = do
    undo <- Txp.processTx tx
    putTxExtraWithHistory id extra $ getTxRelatedAddrs aux undo
    let balanceUpdate = getBalanceUpdate aux undo
    -- TODO: [CSM-245] do not discard logged errors
    fmap fst $ usingLoggerName "eProcessTx" $ runNamedPureLog $
        updateAddrBalances balanceUpdate

-- | Get rid of invalid transactions.
-- All valid transactions will be added to mem pool and applied to utxo.
eNormalizeToil
    :: (ELocalToilMode m)
    => [(TxId, (TxAux, TxExtra))]
    -> m ()
eNormalizeToil txs = mapM_ normalize ordered
  where
    ordered = fromMaybe txs $ topsortTxs wHash txs
    wHash (i, (txAux, _)) = WithHash (taTx txAux) i
    normalize = runExceptT . uncurry eProcessTx . repair
    repair (i, (txAux, extra)) = ((i, txAux), extra)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

data BalanceUpdate = BalanceUpdate
    { minusBalance :: [(Address, Coin)]
    , plusBalance  :: [(Address, Coin)]
    }

data Sign = Plus | Minus

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

getTxRelatedAddrs :: TxAux -> TxUndo -> NonEmpty Address
getTxRelatedAddrs TxAux {taTx = UnsafeTx {..}} undo =
    map txOutAddress _txOutputs `unionNE` map (txOutAddress . toaOut) undo
  where
    toSet = HS.fromList . toList
    -- Safe here, because union of non-empty sets can't be empty.
    unionNE lhs rhs = NE.fromList $ toList $ HS.union (toSet lhs) (toSet rhs)

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
    joiner _ _ = error "combineBalanceUpdates: HashMap map() is broken"
    reducer (Just plus, Just minus)
        | plus > minus = Just (Plus,  unsafeSubCoin plus minus)
        | plus < minus = Just (Minus, unsafeSubCoin minus plus)
    reducer (Nothing, Nothing) = error "combineBalanceUpdates: HashMap unionWith() is broken"
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
    let minusBalance = map (view _TxOut . toaOut) $ toList txUndo
        plusBalance = map (view _TxOut) $ toList $ _txOutputs (taTx txAux)
    in BalanceUpdate {..}
