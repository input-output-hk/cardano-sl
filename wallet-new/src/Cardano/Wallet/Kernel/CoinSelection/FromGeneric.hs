{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.Kernel.CoinSelection.FromGeneric (
    Cardano
  ) where

import           Universum

import qualified Pos.Txp  as Core
import qualified Pos.Core as Core

import           Cardano.Wallet.Kernel.CoinSelection.Generic

data Cardano

instance CoinSelDom Cardano where
  type Input   Cardano = Core.TxIn
  type Output  Cardano = Core.TxOutAux
  type Value   Cardano = Core.Coin

  outVal    = Core.txOutValue . Core.toaOut

  valueZero = Core.mkCoin 0
  valueAdd  = Core.addCoin
  valueSub  = Core.subCoin
  valueMult = Core.mulCoin
  valueDist = \a b -> if a < b then b `Core.unsafeSubCoin` a
                               else a `Core.unsafeSubCoin` b

instance PickFromUtxo Core.Utxo where
  type Dom Core.Utxo = Cardano
