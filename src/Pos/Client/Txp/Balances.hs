{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Client.Txp.Balances
       ( MonadBalances(..)
       ) where

import           Universum

import           Control.Monad.Trans (MonadTrans)
import qualified Data.HashMap.Strict as HM

import qualified Data.Map            as M
import           Pos.DB              (MonadDB)
import qualified Pos.DB.GState       as GS
import           Pos.Txp             (TxOutAux (..), TxpHolder (..), Utxo, addrBelongsTo,
                                      getUtxoModifier, txOutValue)
import           Pos.Types           (Address, Coin, sumCoins, unsafeIntegerToCoin)
import qualified Pos.Util.Modifier   as MM

-- | A class which have the methods to get state of address' balance
class Monad m => MonadBalances m where
    getOwnUtxo :: Address -> m Utxo
    getBalance :: Address -> m Coin
    getBalance addr = unsafeIntegerToCoin . sumCoins .
                      map (txOutValue . toaOut) . toList <$> getOwnUtxo addr
    -- TODO: add a function to get amount of stake (it's different from
    -- balance because of distributions)

    default getOwnUtxo :: (MonadTrans t, MonadBalances m', t m' ~ m) => Address -> m Utxo
    getOwnUtxo = lift . getOwnUtxo

instance {-# OVERLAPPABLE #-}
  (MonadBalances m, MonadTrans t, Monad (t m)) =>
  MonadBalances (t m)

instance (MonadDB m, MonadMask m) => MonadBalances (TxpHolder __ m) where
    getOwnUtxo addr = do
        utxo <- GS.getFilteredUtxo addr
        updates <- getUtxoModifier
        let toDel = MM.deletions updates
            toAdd = HM.filter (`addrBelongsTo` addr) $ MM.insertionsMap updates
            utxo' = foldr M.delete utxo toDel
        return $ HM.foldrWithKey M.insert utxo' toAdd
