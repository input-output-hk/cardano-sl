{-# LANGUAGE TypeFamilies         #-}

module Pos.Client.Txp.Balances
       ( MonadBalances(..)
       , getBalanceFromUtxo
       ) where

import           Universum

import           Control.Monad.Trans    (MonadTrans)
import qualified Data.HashMap.Strict    as HM
import qualified Data.Map               as M
import           Formatting             (sformat, stext, (%))
import           System.Wlog            (WithLogger, logWarning)

import           Pos.Crypto             (WithHash (..), shortHashF)
import           Pos.DB                 (MonadDB)
import qualified Pos.DB.GState          as GS
import qualified Pos.DB.GState.Balances as GS
import           Pos.Txp                (GenericToilModifier (..), TxOutAux (..),
                                         TxpHolder, Utxo, addrBelongsTo, applyToil,
                                         getLocalTxsNUndo, getUtxoModifier, runToilAction,
                                         topsortTxs, txOutValue, _bvStakes)
import           Pos.Types              (Address (..), Coin, mkCoin, sumCoins,
                                         unsafeIntegerToCoin)
import qualified Pos.Util.Modifier      as MM

-- | A class which have the methods to get state of address' balance
class Monad m => MonadBalances m where
    getOwnUtxo :: Address -> m Utxo
    getBalance :: Address -> m Coin
    -- TODO: add a function to get amount of stake (it's different from
    -- balance because of distributions)

    default getOwnUtxo :: (MonadTrans t, MonadBalances m', t m' ~ m) => Address -> m Utxo
    getOwnUtxo = lift . getOwnUtxo

    default getBalance :: (MonadTrans t, MonadBalances m', t m' ~ m) => Address -> m Coin
    getBalance = lift . getBalance

instance {-# OVERLAPPABLE #-}
  (MonadBalances m, MonadTrans t, Monad (t m)) =>
  MonadBalances (t m)

getBalanceFromUtxo :: MonadBalances m => Address -> m Coin
getBalanceFromUtxo addr =
    unsafeIntegerToCoin . sumCoins .
    map (txOutValue . toaOut) . toList <$> getOwnUtxo addr

instance (MonadDB m, MonadMask m, WithLogger m) => MonadBalances (TxpHolder __ m) where
    getOwnUtxo addr = do
        utxo <- GS.getFilteredUtxo addr
        updates <- getUtxoModifier
        let toDel = MM.deletions updates
            toAdd = HM.filter (`addrBelongsTo` addr) $ MM.insertionsMap updates
            utxo' = foldr M.delete utxo toDel
        return $ HM.foldrWithKey M.insert utxo' toAdd

    getBalance PubKeyAddress{..} = do
        (txs, undos) <- getLocalTxsNUndo
        let wHash (i, (t, _, _)) = WithHash t i
        case topsortTxs wHash txs of
            Nothing -> do
                logWarn "couldn't topsort mempool txs"
                getFromDb
            Just ordered -> do
                let txsAndUndos = mapMaybe (\(id, tx) -> (tx,) <$> HM.lookup id undos) ordered
                (_, ToilModifier{..}) <- runToilAction @_ @() (applyToil txsAndUndos)
                let stake = HM.lookup addrKeyHash $ _bvStakes _tmBalances
                maybe getFromDb pure stake
      where
        logWarn er = logWarning $
            sformat ("Couldn't compute balance of "%shortHashF%
                         " using mempool, reason: "%stext) addrKeyHash er
        getFromDb = fromMaybe (mkCoin 0) <$> GS.getRealStake addrKeyHash
    getBalance addr = getBalanceFromUtxo addr
