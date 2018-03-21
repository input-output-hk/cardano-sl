{-# LANGUAGE TemplateHaskell #-}

-- | Pure specification of the wallet
module Wallet.Spec (
    Wallet -- TODO: Not sure if we want to keep this opaque or not
  , walletEmpty
  ) where

import           Universum hiding (State)

import           Control.Lens.TH
import qualified Data.Set as Set
import           Pos.Util

import           UTxO.DSL
import           Wallet.Abstract

{-------------------------------------------------------------------------------
  Representation
-------------------------------------------------------------------------------}

-- | Wallet state
data State h a = State {
      _stateUtxo    :: Utxo h a
    , _statePending :: Pending h a
    }

-- | Wallet is wallet configuration and wallet state
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
        _stateUtxo    = utxoEmpty
      , _statePending = Set.empty
      }

{-------------------------------------------------------------------------------
  IsWallet instance
-------------------------------------------------------------------------------}

instance (Hash h a, Ord a) => IsWallet Wallet h a where
  utxo = view (walletState . stateUtxo)
  ours = view walletOurs

  applyBlock :: Block h a -> Wallet h a -> Wallet h a
  applyBlock b w = w & walletState . stateUtxo    %~ updateUtxo (ours w) b
                     & walletState . statePending %~ updatePending       b
