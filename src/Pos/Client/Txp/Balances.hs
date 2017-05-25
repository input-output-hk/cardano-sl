{-# LANGUAGE TypeFamilies #-}

module Pos.Client.Txp.Balances
       ( MonadBalances(..)
       , getOwnUtxo
       , getBalanceFromUtxo
       , BalancesRedirect
       , runBalancesRedirect
       ) where

import           Universum

import           Control.Monad.Trans          (MonadTrans)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as HS
import qualified Data.Map                     as M
import qualified Ether
import           Formatting                   (sformat, stext, (%))
import           System.Wlog                  (WithLogger, logWarning)

import           Pos.Core                     (AddressIgnoringAttributes (AddressIA))
import           Pos.Crypto                   (WithHash (..), shortHashF)
import           Pos.DB                       (MonadDB, MonadDBPure)
import qualified Pos.DB.GState                as GS
import qualified Pos.DB.GState.Balances       as GS
import           Pos.Txp                      (GenericToilModifier (..), MonadTxpMem,
                                               TxAux (..), TxOutAux (..), Utxo,
                                               addrBelongsToSet, applyToil,
                                               getLocalTxsNUndo, getUtxoModifier,
                                               runToilAction, topsortTxs, txOutValue,
                                               _bvStakes)
import           Pos.Types                    (Address (..), Coin, mkCoin, sumCoins,
                                               unsafeIntegerToCoin)
import qualified Pos.Util.Modifier            as MM

-- | A class which have the methods to get state of address' balance
class Monad m => MonadBalances m where
    getOwnUtxos :: [Address] -> m Utxo
    getBalance :: Address -> m Coin
    -- TODO: add a function to get amount of stake (it's different from
    -- balance because of distributions)

    default getOwnUtxos :: (MonadTrans t, MonadBalances m', t m' ~ m) => [Address] -> m Utxo
    getOwnUtxos = lift . getOwnUtxos

    default getBalance :: (MonadTrans t, MonadBalances m', t m' ~ m) => Address -> m Coin
    getBalance = lift . getBalance

instance {-# OVERLAPPABLE #-}
    (MonadBalances m, MonadTrans t, Monad (t m)) =>
        MonadBalances (t m)

getBalanceFromUtxo :: MonadBalances m => Address -> m Coin
getBalanceFromUtxo addr =
    unsafeIntegerToCoin . sumCoins .
    map (txOutValue . toaOut) . toList <$> getOwnUtxo addr

data BalancesRedirectTag

type BalancesRedirect =
    Ether.TaggedTrans BalancesRedirectTag IdentityT

runBalancesRedirect :: BalancesRedirect m a -> m a
runBalancesRedirect = coerce

instance
    (MonadDB m, MonadDBPure m, MonadMask m, WithLogger m, MonadTxpMem ext m, t ~ IdentityT) =>
        MonadBalances (Ether.TaggedTrans BalancesRedirectTag t m)
  where
    getOwnUtxos addr = do
        utxo <- GS.getFilteredUtxo addr
        updates <- getUtxoModifier
        let addrsSet = HS.fromList $ AddressIA <$> addr
            toDel    = MM.deletions updates
            toAdd    = HM.filter (`addrBelongsToSet` addrsSet) $
                       MM.insertionsMap updates
            utxo'    = foldr M.delete utxo toDel
        return $ HM.foldrWithKey M.insert utxo' toAdd

    getBalance PubKeyAddress{..} = do
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
    getBalance addr = getBalanceFromUtxo addr

getOwnUtxo :: MonadBalances m => Address -> m Utxo
getOwnUtxo = getOwnUtxos . one
