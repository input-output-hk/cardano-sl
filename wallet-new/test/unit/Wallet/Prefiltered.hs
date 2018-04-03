{-# LANGUAGE TemplateHaskell #-}

-- | Wallet with incremental balance updates and prefiltering
module Wallet.Prefiltered (
    Wallet -- opaque
  , walletEmpty
  ) where

import           Universum

import           Control.Lens.TH
import qualified Data.Set as Set
import           Pos.Util

import           UTxO.DSL
import           Wallet.Abstract
import qualified Wallet.Incremental as Incr

newtype Wallet h a = Prefiltered { _incremental :: Incr.Wallet h a }

makeLenses ''Wallet

walletEmpty :: Ours a -> Wallet h a
walletEmpty = Prefiltered . Incr.walletEmpty

instance HasLens (Pending h a) (Wallet h a) (Pending h a) where
  lensOf = incremental . lensOf'

instance (Hash h a, Ord a) => IsWallet Wallet h a where
  utxo = utxo . view incremental
  ours = ours . view incremental

  applyBlock b w = w & (incremental . Incr.walletState) %~
      Incr.applyBlock' (
          txIns b `Set.intersection` utxoDomain (utxo w `utxoUnion` utxoNew)
        , utxoNew
        )
    where
      utxoNew = utxoRestrictToOurs (ours w) (txOuts b)

  -- TODO: It's annoying that we have to redefine these. We should inherit
  -- them from the incremental wallet.

  availableBalance :: Wallet h a -> Value
  availableBalance = availableBalance . _incremental

  totalBalance :: Wallet h a -> Value
  totalBalance = totalBalance . _incremental
