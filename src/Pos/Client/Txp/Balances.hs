{-# LANGUAGE TypeFamilies #-}

module Pos.Client.Txp.Balances
       ( MonadBalances(..)
       , getBalanceFromUtxo
       ) where

import           Universum

import           Control.Monad.Trans         (MonadTrans)
import qualified Data.HashMap.Strict         as HM

import qualified Data.Map                    as M
import           Pos.Communication.PeerState (PeerStateHolder)
import qualified Pos.Context                 as PC
import           Pos.DB                      (MonadDB)
import qualified Pos.DB.GState               as GS
import qualified Pos.DB.GState.Balances      as GS
import           Pos.Delegation              (DelegationT (..))
import           Pos.DHT.Real                (KademliaDHT (..))
import           Pos.Slotting                (NtpSlotting, SlottingHolder)
import           Pos.Ssc.Extra               (SscHolder (..))
import           Pos.Txp                     (TxOutAux (..), TxpHolder (..), Utxo,
                                              addrBelongsTo, getUtxoModifier, txOutValue)
import           Pos.Types                   (Address (..), Coin, mkCoin, sumCoins,
                                              unsafeIntegerToCoin)
import qualified Pos.Util.Modifier           as MM

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

getBalanceFromUtxo :: MonadBalances m => Address -> m Coin
getBalanceFromUtxo addr =
    unsafeIntegerToCoin . sumCoins .
    map (txOutValue . toaOut) . toList <$> getOwnUtxo addr

instance MonadBalances m => MonadBalances (ReaderT r m)
instance MonadBalances m => MonadBalances (StateT s m)
instance MonadBalances m => MonadBalances (KademliaDHT m)
instance MonadBalances m => MonadBalances (PeerStateHolder m)
instance MonadBalances m => MonadBalances (NtpSlotting m)
instance MonadBalances m => MonadBalances (SlottingHolder m)

deriving instance MonadBalances m => MonadBalances (PC.ContextHolder ssc m)
deriving instance MonadBalances m => MonadBalances (SscHolder ssc m)
deriving instance MonadBalances m => MonadBalances (DelegationT m)

instance (MonadDB m, MonadMask m) => MonadBalances (TxpHolder __ m) where
    getOwnUtxo addr = do
        utxo <- GS.getFilteredUtxo addr
        updates <- getUtxoModifier
        let toDel = MM.deletions updates
            toAdd = HM.filter (`addrBelongsTo` addr) $ MM.insertionsMap updates
            utxo' = foldr M.delete utxo toDel
        return $ HM.foldrWithKey M.insert utxo' toAdd

    getBalance PubKeyAddress{..} =
        fromMaybe (mkCoin 0) <$> GS.getRealStake addrKeyHash
    getBalance addr = getBalanceFromUtxo addr
