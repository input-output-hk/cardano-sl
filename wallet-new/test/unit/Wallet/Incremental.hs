{-# LANGUAGE TemplateHaskell #-}

-- | Incremental yet pure version of the wallet
--
-- This is intended to be one step between the spec and the implementation.
-- We provide it here so that we can quickcheck this, and then base the
-- real implementation on this incremental version.
module Wallet.Incremental (
    Wallet
  , walletEmpty
    -- * Internals
  , applyBlock'
  , walletState
  ) where

import           Universum hiding (State)

import           Control.Lens.TH
import qualified Data.Set as Set
import           Pos.Util

import           Util
import           UTxO.DSL
import           Wallet.Abstract

{-------------------------------------------------------------------------------
  Representation
-------------------------------------------------------------------------------}

-- | Wallet state
--
-- Invariant:
--
-- > stateUtxoBalance == balance stateUtxo
data State h a = State {
      _stateUtxo        :: Utxo h a
    , _stateUtxoBalance :: Value
    , _statePending     :: Pending h a
    }

-- | Wallet
data Wallet h a = Wallet {
      _walletOurs  :: Ours a
    , _walletState :: State h a
    }

makeLenses ''State
makeLenses ''Wallet

instance HasLens (Pending h a) (Wallet h a) (Pending h a) where
    lensOf = walletState . statePending

-- | Wallet state
walletEmpty :: Ours a -> Wallet h a
walletEmpty _walletOurs = Wallet {..}
  where
    _walletState = State {
        _stateUtxo        = utxoEmpty
      , _stateUtxoBalance = 0
      , _statePending     = Set.empty
      }

{-------------------------------------------------------------------------------
  IsWallet instance
-------------------------------------------------------------------------------}

instance (Hash h a, Ord a) => IsWallet Wallet h a where
  utxo = view (walletState . stateUtxo)
  ours = view walletOurs

  applyBlock b w = w & walletState %~ applyBlock' (txIns b, utxoNew)
    where
      utxoNew = utxoRestrictToOurs (ours w) (txOuts b)

  -- We can also replace some default methods with more efficient versions

  availableBalance :: Wallet h a -> Value
  availableBalance w =
        (w ^. walletState . stateUtxoBalance)
      - balance (utxoRestrictToInputs (txIns (pending w)) (utxo w))

  totalBalance :: Wallet h a -> Value
  totalBalance w = availableBalance w + balance (change w)

applyBlock' :: Hash h a => (Set (Input h a), Utxo h a) -> State h a -> State h a
applyBlock' (ins, outs) State{..} = State {
      _stateUtxo        = utxo'
    , _stateUtxoBalance = balance'
    , _statePending     = pending'
    }
  where
    pending' = Set.filter (\t -> disjoint (trIns t) ins) _statePending
    utxoNew  = outs
    unionNew = _stateUtxo `utxoUnion` utxoNew
    utxoRem  = utxoRestrictToInputs ins unionNew
    utxo'    = utxoRemoveInputs     ins unionNew
    balance' = _stateUtxoBalance + balance utxoNew - balance utxoRem
