-- | Incremental yet pure version of the wallet
--
-- This is intended to be one step between the spec and the implementation.
-- We provide it here so that we can quickcheck this, and then base the
-- real implementation on this incremental version.
module Wallet.Incremental (
    Wallet
  , walletEmpty
  ) where

import Universum hiding (State)
import qualified Data.Set as Set

import UTxO.DSL
import Wallet.Abstract

{-------------------------------------------------------------------------------
  Representation
-------------------------------------------------------------------------------}

-- | Wallet state
--
-- Invariant:
--
-- > stateUtxoBalance == balance stateUtxo
data State h a = State {
      stateUtxo        :: Utxo h a
    , stateUtxoBalance :: Value
    , statePending     :: Pending h a
    }

-- | Wallet
data Wallet h a = Wallet {
      walletOurs  :: Ours a
    , walletState :: State h a
    }

-- | Wallet state
walletEmpty :: Ours a -> Wallet h a
walletEmpty walletOurs = Wallet {..}
  where
    walletState = State {
        stateUtxo        = utxoEmpty
      , stateUtxoBalance = 0
      , statePending     = Set.empty
      }

-- | Internal helper function for updating wallet state
updateState :: Functor f
            => (State h a -> f (State h a))
            -> Wallet h a -> f (Wallet h a)
updateState f w = (\st -> w { walletState = st }) <$> f (walletState w)

{-------------------------------------------------------------------------------
  IsWallet instance
-------------------------------------------------------------------------------}

instance (Hash h a, Ord a) => IsWallet Wallet h a where
  pending :: Wallet h a -> Pending h a
  pending = statePending . walletState

  utxo :: Wallet h a -> Utxo h a
  utxo = stateUtxo . walletState

  ours :: Wallet h a -> Ours a
  ours = walletOurs

  applyBlock :: Block h a -> Wallet h a -> Wallet h a
  applyBlock b w = runIdentity $ updateState aux w
    where
     aux :: State h a -> Identity (State h a)
     aux State{..} = return State {
           stateUtxo        = utxo'
         , stateUtxoBalance = balance'
         , statePending     = pending'
         }
       where
         pending' = updatePending b statePending
         utxoNew  = utxoRestrictToOurs (ours w) (txOuts b)
         unionNew = stateUtxo `utxoUnion` utxoNew
         utxoRem  = utxoRestrictToInputs (txIns b) unionNew
         utxo'    = utxoRemoveInputs     (txIns b) unionNew
         balance' = stateUtxoBalance + balance utxoNew - balance utxoRem

  newPending :: Transaction h a -> Wallet h a -> Maybe (Wallet h a)
  newPending tx w = updateState aux w
    where
      aux :: State h a -> Maybe (State h a)
      aux State{..} = do
          guard $ trIns tx `Set.isSubsetOf` utxoDomain (available w)
          return State {
              stateUtxo        = stateUtxo
            , stateUtxoBalance = stateUtxoBalance
            , statePending     = Set.insert tx statePending
            }

  -- We can also replace some default methods with more efficient versions

  availableBalance :: Wallet h a -> Value
  availableBalance Wallet{..} =
        stateUtxoBalance
      - balance (utxoRestrictToInputs (txIns statePending) stateUtxo)
    where
      State{..} = walletState

  totalBalance :: Wallet h a -> Value
  totalBalance w = availableBalance w + balance (change w)
