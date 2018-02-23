{-# LANGUAGE InstanceSigs #-}

-- | Pure specification of the wallet
module Wallet.Spec (
    Wallet -- TODO: Not sure if we want to keep this opaque or not
  , walletEmpty
  ) where

import qualified Data.Set as Set
import           Universum hiding (State)

import           UTxO.DSL
import           Wallet.Abstract

{-------------------------------------------------------------------------------
  Representation
-------------------------------------------------------------------------------}

-- | Wallet state
data State h a = State {
      stateUtxo    :: Utxo h a
    , statePending :: Pending h a
    }

-- | Wallet is wallet configuration and wallet state
data Wallet h a = Wallet {
      walletOurs  :: Ours a
    , walletState :: State h a
    }

-- | Wallet state
walletEmpty :: Ours a -> Wallet h a
walletEmpty walletOurs = Wallet {..}
  where
    walletState = State {
        stateUtxo    = utxoEmpty
      , statePending = Set.empty
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
           stateUtxo    = updateUtxo (ours w) b stateUtxo
         , statePending = updatePending       b statePending
         }

  newPending :: Transaction h a -> Wallet h a -> Maybe (Wallet h a)
  newPending tx w = updateState aux w
    where
      aux :: State h a -> Maybe (State h a)
      aux State{..} = do
          guard $ trIns tx `Set.isSubsetOf` utxoDomain (available w)
          return State {
              stateUtxo    = stateUtxo
            , statePending = Set.insert tx statePending
            }

updateUtxo :: forall h a. Hash h a
           => Ours a -> Block h a -> Utxo h a -> Utxo h a
updateUtxo p b = remSpent . addNew
  where
    addNew, remSpent :: Utxo h a -> Utxo h a
    addNew   = utxoUnion (utxoRestrictToOurs p (txOuts b))
    remSpent = utxoRemoveInputs (txIns b)
