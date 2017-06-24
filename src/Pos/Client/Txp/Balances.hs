{-# LANGUAGE TypeFamilies #-}

module Pos.Client.Txp.Balances
       ( MonadBalances(..)
       , getOwnUtxo
       , getBalanceFromUtxo
       , getOwnUtxosDefault
       , getBalanceDefault
       ) where

import           Universum

import           Control.Monad.Trans    (MonadTrans)
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS
import qualified Data.Map               as M
import           Formatting             (sformat, stext, (%))
import           System.Wlog            (WithLogger, logWarning)

import           Pos.Core               (AddressIgnoringAttributes (AddressIA))
import           Pos.Crypto             (WithHash (..), shortHashF)
import           Pos.DB                 (MonadDBRead, MonadGState, MonadRealDB)
import qualified Pos.DB.GState          as GS
import qualified Pos.DB.GState.Balances as GS
import           Pos.Txp                (GenericToilModifier (..), MonadTxpMem,
                                         TxAux (..), TxOutAux (..), Utxo,
                                         addrBelongsToSet, applyToil, getLocalTxsNUndo,
                                         getUtxoModifier, runToilAction, topsortTxs,
                                         txOutValue, _bvStakes)
import           Pos.Types              (Address (..), Coin, mkCoin, sumCoins,
                                         unsafeIntegerToCoin)
import qualified Pos.Util.Modifier      as MM

-- | A class which have the methods to get state of address' balance
class Monad m => MonadBalances m where
    getOwnUtxos :: [Address] -> m Utxo
    getBalance :: Address -> m Coin
    -- TODO: add a function to get amount of stake (it's different from
    -- balance because of distributions)

instance {-# OVERLAPPABLE #-}
    (MonadBalances m, MonadTrans t, Monad (t m)) =>
        MonadBalances (t m)
  where
    getOwnUtxos = lift . getOwnUtxos
    getBalance = lift . getBalance


getBalanceFromUtxo :: MonadBalances m => Address -> m Coin
getBalanceFromUtxo addr =
    unsafeIntegerToCoin . sumCoins .
    map (txOutValue . toaOut) . toList <$> getOwnUtxo addr

type BalancesEnv ext m =
    (MonadRealDB m, MonadDBRead m, MonadGState m, MonadMask m, WithLogger m, MonadTxpMem ext m)

getOwnUtxosDefault :: BalancesEnv ext m => [Address] -> m Utxo
getOwnUtxosDefault addr = do
    utxo <- GS.getFilteredUtxo addr
    updates <- getUtxoModifier
    let addrsSet = HS.fromList $ AddressIA <$> addr
        toDel    = MM.deletions updates
        toAdd    = HM.filter (`addrBelongsToSet` addrsSet) $
                   MM.insertionsMap updates
        utxo'    = foldr M.delete utxo toDel
    return $ HM.foldrWithKey M.insert utxo' toAdd

getBalanceDefault :: (BalancesEnv ext m, MonadBalances m) => Address -> m Coin
getBalanceDefault PubKeyAddress{..} = do
    (txs, undos) <- getLocalTxsNUndo
    let wHash (i, TxAux tx _ _) = WithHash tx i
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
getBalanceDefault addr = getBalanceFromUtxo addr

getOwnUtxo :: MonadBalances m => Address -> m Utxo
getOwnUtxo = getOwnUtxos . one
