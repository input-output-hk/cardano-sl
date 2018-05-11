{-# LANGUAGE TemplateHaskell #-}

-- | Pure specification of the wallet
--
-- NOTE: All wallet implements are intended to be imported qualified.
module Wallet.Basic (
    -- * State
    State(..)
  , stateUtxo
  , statePending
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

import           UTxO.DSL
import           Wallet.Abstract

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

data State h a = State {
      _stateUtxo    :: Utxo h a
    , _statePending :: Pending h a
    }

makeLenses ''State

initState :: State h a
initState = State {
      _stateUtxo    = utxoEmpty
    , _statePending = Set.empty
    }

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

mkWallet :: (Hash h a, Ord a, Buildable st)
         => Ours a -> Lens' st (State h a) -> WalletConstr h a st
mkWallet ours l self st = (mkDefaultWallet (l . statePending) self st) {
      utxo       = st ^. l . stateUtxo
    , ours       = ours
    , applyBlock = \b -> self (st & l %~ applyBlock' ours b)
    }

walletEmpty :: (Hash h a, Ord a, Buildable a) => Ours a -> Wallet h a
walletEmpty ours = fix (mkWallet ours identity) initState

{-------------------------------------------------------------------------------
  Implementation
-------------------------------------------------------------------------------}

applyBlock' :: Hash h a
            => Ours a -> Block h a -> State h a -> State h a
applyBlock' ours b State{..} = State {
      _stateUtxo    = updateUtxo ours b _stateUtxo
    , _statePending = updatePending   b _statePending
    }

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance (Hash h a, Buildable a) => Buildable (State h a) where
  build State{..} = bprint
    ( "State"
    % "{ utxo:    " % build
    % ", pending: " % build
    % "}"
    )
    _stateUtxo
    _statePending
