{-# LANGUAGE TemplateHaskell #-}

-- | Wallet with incremental balance updates and prefiltering
module Wallet.Prefiltered (
    -- * State (re-exported from "Wallet.Incremental")
    Incr.State
  , Incr.stateUtxo
  , Incr.statePending
  , Incr.stateUtxoBalance
  , Incr.initState
    -- Construction
  , mkWallet
  , walletEmpty
  ) where

import           Universum

import qualified Data.Set as Set

import           UTxO.DSL
import           Wallet.Abstract
import qualified Wallet.Incremental as Incr

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

mkWallet :: (Hash h a, Ord a, Buildable st)
         => Ours a -> Lens' st (Incr.State h a) -> WalletConstr h a st
mkWallet ours l self st = (Incr.mkWallet ours l self st) {
      applyBlock = \b ->
        let utxoPlus   = utxoRestrictToOurs ours (txOuts b)
            filterUtxo = utxo this `utxoUnion` utxoPlus
            filtered   = txIns b `Set.intersection` utxoDomain filterUtxo
        in self (st & l %~ Incr.applyBlock' (filtered, utxoPlus))
    }
  where
    this = self st

walletEmpty :: (Hash h a, Ord a, Buildable a) => Ours a -> Wallet h a
walletEmpty ours = fix (mkWallet ours identity) Incr.initState
