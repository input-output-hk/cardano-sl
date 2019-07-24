{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

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

import           Pos.Chain.Block (HeaderHash)
import           Pos.Chain.Genesis (GenesisWStakeholders)
import           Pos.Chain.Txp (ToilVerFailure (..), Tx (..), TxAux (..), TxId,
                     TxOut (..), TxOutAux (..), TxUndo, TxValidationRules,
                     TxpConfiguration, extendGlobalToilM, extendLocalToilM,
                     topsortTxs, _TxOut)
import qualified Pos.Chain.Txp as Txp
import           Pos.Chain.Update (BlockVersionData)
import           Pos.Core (Address, Coin, EpochIndex, Timestamp, mkCoin,
                     sumCoins, unsafeAddCoin, unsafeSubCoin)
import           Pos.Core.Chrono (NewestFirst (..))
import           Pos.Crypto (ProtocolMagic, WithHash (..), hash)
import           Pos.Explorer.Core (AddrHistory, TxExtra (..))
import           Pos.Explorer.Txp.Toil.Monad (EGlobalToilM, ELocalToilM,
                     ExplorerExtraM)
import qualified Pos.Explorer.Txp.Toil.Monad as ToilM
import           Pos.Util.Util (Sign (..))
import           Pos.Util.Wlog (logError)

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

-- | Apply transactions from one block. They must be valid (for
-- example, it implies topological sort).
eApplyToil
    :: GenesisWStakeholders
    -> Maybe Timestamp
    -> [(TxAux, TxUndo)]
    -> HeaderHash
    -> EGlobalToilM ()
eApplyToil bootStakeholders mTxTimestamp txun hh = do
    extendGlobalToilM $ Txp.applyToil bootStakeholders txun
    ToilM.explorerExtraMToEGlobalToilM $ mapM_ applier $ zip [0..] txun
  where
    applier :: (Word32, (TxAux, TxUndo)) -> ExplorerExtraM ()
    applier (i, (txAux, txUndo)) = do
        let tx = taTx txAux
            id = hash tx
            newExtra = TxExtra (Just (hh, i)) mTxTimestamp txUndo
        extra <- fromMaybe newExtra <$> ToilM.getTxExtra id
        putTxExtraWithHistory id extra $ getTxRelatedAddrs txAux txUndo
        let balanceUpdate = getBalanceUpdate txAux txUndo
        updateAddrBalances balanceUpdate
        updateUtxoSumFromBalanceUpdate balanceUpdate

-- | Rollback transactions from one block.
eRollbackToil :: GenesisWStakeholders -> [(TxAux, TxUndo)] -> EGlobalToilM ()
eRollbackToil bootStakeholders txun = do
    extendGlobalToilM $ Txp.rollbackToil bootStakeholders txun
    ToilM.explorerExtraMToEGlobalToilM $ mapM_ extraRollback $ reverse txun
  where
    extraRollback :: (TxAux, TxUndo) -> ExplorerExtraM ()
    extraRollback (txAux, txUndo) = do
        delTxExtraWithHistory (hash (taTx txAux)) $
          getTxRelatedAddrs txAux txUndo
        let balanceUpdate = flipBalanceUpdate $ getBalanceUpdate txAux txUndo
        updateAddrBalances balanceUpdate
        updateUtxoSumFromBalanceUpdate balanceUpdate

----------------------------------------------------------------------------
-- Local
----------------------------------------------------------------------------

-- | Verify one transaction and also add it to mem pool and apply to utxo
-- if transaction is valid.
eProcessTx
    :: ProtocolMagic
    -> TxValidationRules
    -> TxpConfiguration
    -> BlockVersionData
    -> EpochIndex
    -> (TxId, TxAux)
    -> (TxUndo -> TxExtra)
    -> ExceptT ToilVerFailure ELocalToilM ()
eProcessTx pm txValRules txpConfig bvd curEpoch tx@(id, aux) createExtra = do
    undo <- mapExceptT extendLocalToilM $ Txp.processTx pm txValRules txpConfig bvd curEpoch tx
    lift $ ToilM.explorerExtraMToELocalToilM $ do
        let extra = createExtra undo
        putTxExtraWithHistory id extra $ getTxRelatedAddrs aux undo
        let balanceUpdate = getBalanceUpdate aux undo
        updateAddrBalances balanceUpdate
        updateUtxoSumFromBalanceUpdate balanceUpdate

-- | Get rid of invalid transactions.
-- All valid transactions will be added to mem pool and applied to utxo.
eNormalizeToil
    :: ProtocolMagic
    -> TxValidationRules
    -> TxpConfiguration
    -> BlockVersionData
    -> EpochIndex
    -> [(TxId, (TxAux, TxExtra))]
    -> ELocalToilM ()
eNormalizeToil pm txValRules txpConfig bvd curEpoch txs = mapM_ normalize ordered
  where
    ordered = fromMaybe txs $ topsortTxs wHash txs
    wHash (i, (txAux, _)) = WithHash (taTx txAux) i
    normalize = runExceptT . uncurry (eProcessTx pm txValRules txpConfig bvd curEpoch) . repair
    repair (i, (txAux, extra)) = ((i, txAux), const extra)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

data BalanceUpdate = BalanceUpdate
    { minusBalance :: [(Address, Coin)]
    , plusBalance  :: [(Address, Coin)]
    }

-- | Flip the plus and minus balances, so that a flipped BalanceUpdate
-- is a rollback.
flipBalanceUpdate :: BalanceUpdate -> BalanceUpdate
flipBalanceUpdate initial =
    BalanceUpdate
        { plusBalance = minusBalance initial
        , minusBalance = plusBalance initial
        }

modifyAddrHistory :: (AddrHistory -> AddrHistory) -> Address -> ExplorerExtraM ()
modifyAddrHistory f addr = ToilM.updateAddrHistory addr . f =<< ToilM.getAddrHistory addr

putTxExtraWithHistory :: TxId -> TxExtra -> NonEmpty Address -> ExplorerExtraM ()
putTxExtraWithHistory id extra addrs = do
    ToilM.putTxExtra id extra
    for_ addrs $ modifyAddrHistory $
        NewestFirst . (id :) . getNewestFirst

delTxExtraWithHistory :: TxId -> NonEmpty Address -> ExplorerExtraM ()
delTxExtraWithHistory id addrs = do
    ToilM.delTxExtra id
    for_ addrs $ modifyAddrHistory $
        NewestFirst . delete id . getNewestFirst

updateUtxoSumFromBalanceUpdate :: BalanceUpdate -> ExplorerExtraM ()
updateUtxoSumFromBalanceUpdate balanceUpdate = do
    let plusChange  = sumCoins $ map snd $ plusBalance  balanceUpdate
        minusChange = sumCoins $ map snd $ minusBalance balanceUpdate
        utxoChange  = plusChange - minusChange
    utxoSum <- ToilM.getUtxoSum
    ToilM.putUtxoSum $ utxoSum + utxoChange

getTxRelatedAddrs :: TxAux -> TxUndo -> NonEmpty Address
getTxRelatedAddrs TxAux {taTx = UnsafeTx {..}} undos =
    map txOutAddress _txOutputs
        `unionNEnList` map (txOutAddress . toaOut) (catMaybes $ toList undos)
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
        -- The only otherwise case is actually 'plus == minus'.
        | otherwise = Nothing
    reducer (Just plus, Nothing) = if plus /= mkCoin 0 then Just (Plus, plus) else Nothing
    reducer (Nothing, Just minus) = if minus /= mkCoin 0 then Just (Minus, minus) else Nothing
    reducer (Nothing, Nothing) = error "combineBalanceUpdates: HashMap.unionWith is broken"

updateAddrBalances :: BalanceUpdate -> ExplorerExtraM ()
updateAddrBalances balances = mapM_ updater $ combineBalanceUpdates balances
  where
    updater :: (Address, (Sign, Coin)) -> ExplorerExtraM ()
    updater (addr, (Plus, coin)) = do
        currentBalance <- fromMaybe (mkCoin 0) <$> ToilM.getAddrBalance addr
        let newBalance = unsafeAddCoin currentBalance coin
        ToilM.putAddrBalance addr newBalance
    updater (addr, (Minus, coin)) = do
        maybeBalance <- ToilM.getAddrBalance addr
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
                | otherwise ->
                    ToilM.putAddrBalance addr (unsafeSubCoin currentBalance coin)

getBalanceUpdate :: TxAux -> TxUndo -> BalanceUpdate
getBalanceUpdate txAux txUndo =
    let minusBalance = map (view _TxOut . toaOut) $ catMaybes $ toList txUndo
        plusBalance = map (view _TxOut) $ toList $ _txOutputs (taTx txAux)
    in BalanceUpdate {..}
