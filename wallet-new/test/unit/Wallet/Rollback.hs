{-# LANGUAGE TemplateHaskell #-}

-- | Specification of rollback
module Wallet.Rollback (
    Wallet -- opaque
  , walletEmpty
  ) where

import           Universum hiding (State, head)

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
    , _walletState :: [State h a]
    }

makeLenses ''State
makeLenses ''Wallet

currentState :: Lens' (Wallet h a) (State h a)
currentState = walletState . head

instance HasLens (Pending h a) (Wallet h a) (Pending h a) where
    lensOf = currentState . statePending

-- | Wallet state
walletEmpty :: Ours a -> Wallet h a
walletEmpty _walletOurs = Wallet {_walletState = [st], ..}
  where
    st = State {
        _stateUtxo    = utxoEmpty
      , _statePending = Set.empty
      }

{-------------------------------------------------------------------------------
  IsWallet and Rollback instances
-------------------------------------------------------------------------------}

instance (Hash h a, Ord a) => IsWallet Wallet h a where
  utxo = view (currentState . stateUtxo)
  ours = view walletOurs

  applyBlock :: Block h a -> Wallet h a -> Wallet h a
  applyBlock b w = w & walletState %~ (newState :)
    where
      newState :: State h a
      newState = State {
            _stateUtxo    = updateUtxo (ours w) b (w ^. currentState . stateUtxo)
          , _statePending = updatePending       b (w ^. currentState . statePending)
          }

  -- TODO: This version is incorrect. We need to modify the definition of
  -- 'change' (see section 7.3., "Available and change", of the spec.)
  -- Intentially leaving this bug in here for now so that we can make sure
  -- unit tests catch it before fixing it.

instance (Hash h a, Ord a) => Rollback Wallet h a where
  rollback w = w & walletState %~ aux
    where
      aux :: [State h a] -> [State h a]
      aux (cur:prev:prevs) = cur':prevs
        where
          cur' = State {
                _stateUtxo    = prev ^. stateUtxo
              , _statePending = Set.unions [
                                    cur  ^. statePending
                                  , prev ^. statePending
                                  ]
              }
      aux _ = error "cannot rollback"

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

head :: Lens' [a] a
head f = \(x:xs) -> (:xs) <$> f x
