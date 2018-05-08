{-# LANGUAGE TemplateHaskell #-}

-- | Incremental yet pure version of the wallet
--
-- This is intended to be one step between the spec and the implementation.
-- We provide it here so that we can quickcheck this, and then base the
-- real implementation on this incremental version.
module Wallet.Incremental (
    -- * State
    State(..)
  , stateUtxo
  , statePending
  , stateUtxoBalance
  , initState
    -- * Construction
  , mkWallet
  , walletEmpty
    -- * Implementation
  , applyBlock'
  ) where

import           Universum hiding (State)

import           Control.Lens.TH
import qualified Data.Set as Set
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))

import           Util
import           UTxO.DSL
import           Wallet.Abstract
import qualified Wallet.Basic as Basic

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

-- Invariant:
--
-- > stateUtxoBalance == balance stateUtxo
data State h a = State {
      _stateBasic       :: Basic.State h a
    , _stateUtxoBalance :: Value
    }

makeLenses ''State

stateUtxo :: Lens' (State h a) (Utxo h a)
stateUtxo = stateBasic . Basic.stateUtxo

statePending :: Lens' (State h a) (Pending h a)
statePending = stateBasic . Basic.statePending

initState :: State h a
initState = State {
      _stateBasic       = Basic.initState
    , _stateUtxoBalance = 0
    }

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

mkWallet :: (Hash h a, Ord a, Buildable st)
         => Ours a -> Lens' st (State h a) -> WalletConstr h a st
mkWallet ours l self st = (Basic.mkWallet ours (l . stateBasic) self st) {
      applyBlock = \b ->
        let utxoPlus = utxoRestrictToOurs ours (txOuts b)
        in self (st & l %~ applyBlock' (txIns b, utxoPlus))
    , availableBalance =
          (st ^. l . stateUtxoBalance)
        - balance (utxoRestrictToInputs (txIns (pending this)) (utxo this))
    , totalBalance = availableBalance this + balance (change this)
    }
  where
    this = self st

walletEmpty :: (Hash h a, Ord a, Buildable a) => Ours a -> Wallet h a
walletEmpty ours = fix (mkWallet ours identity) initState

{-------------------------------------------------------------------------------
  Implementation
-------------------------------------------------------------------------------}

applyBlock' :: Hash h a
            => (Set (Input h a), Utxo h a)
            -> State h a -> State h a
applyBlock' (ins, outs) State{..} = State{
      _stateBasic = Basic.State {
          _statePending = pending'
        , _stateUtxo    = utxo'
        }
    , _stateUtxoBalance = balance'
    }
  where
    pending' = Set.filter (\t -> disjoint (trIns t) ins) _statePending
    utxoNew  = outs
    unionNew = _stateUtxo `utxoUnion` utxoNew
    utxoRem  = utxoRestrictToInputs ins unionNew
    utxo'    = utxoRemoveInputs     ins unionNew
    balance' = _stateUtxoBalance + balance utxoNew - balance utxoRem

    Basic.State{..} = _stateBasic

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance (Hash h a, Buildable a) => Buildable (State h a) where
  build State{..} = bprint
    ( "State"
    % "{ basic:       " % build
    % ", utxoBalance: " % build
    % "}"
    )
    _stateBasic
    _stateUtxoBalance
