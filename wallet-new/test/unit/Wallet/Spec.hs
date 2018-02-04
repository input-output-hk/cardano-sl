-- | Pure specification of the wallet
module Wallet.Spec (
    -- * Types
    Ours
  , Pending
  , Wallet(..)
  , walletEmpty
    -- * Main wallet operations
  , totalBalance
  , availableBalance
  , applyBlock
  , newPending
    -- * Properties
    -- ** Invariants
  , InductiveWallet(..)
  , invariant
  , pendingInUtxo
  , utxoIsOurs
  , changeNotAvailable
  , changeNotInUtxo
  , changeAvailable
  , balanceChangeAvailable
  ) where

import Universum
import qualified Data.Foldable   as Fold
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import UTxO.DSL
import UTxO.Crypto

{-------------------------------------------------------------------------------
  Wallet types
-------------------------------------------------------------------------------}

-- | Check if an address is ours
type Ours a = a -> Maybe SomeKeyPair

-- | Pending transactions
type Pending h a = Set (Transaction h a)

-- | Wallet state
data Wallet h a = Wallet {
      walletUtxo    :: Utxo h a
    , walletPending :: Pending h a
    }

walletEmpty :: Wallet h a
walletEmpty = Wallet {
      walletUtxo    = utxoEmpty
    , walletPending = Set.empty
    }

{-------------------------------------------------------------------------------
  Main wallet operations

  TODO: Undo operations currently ignored
-------------------------------------------------------------------------------}

totalBalance :: Hash h a => Ours a -> Wallet h a -> Value
totalBalance ours = balance . total ours

availableBalance :: Hash h a => Wallet h a -> Value
availableBalance = balance . available

applyBlock :: Hash h a => Ours a -> Block h a -> Wallet h a -> Wallet h a
applyBlock ours b Wallet{..} = Wallet{
      walletUtxo    = updateUtxo ours b walletUtxo
    , walletPending = updatePending   b walletPending
    }

newPending :: (Hash h a, Ord a)
           => Transaction h a -> Wallet h a -> Maybe (Wallet h a)
newPending tx w@Wallet{..} = do
    guard $ trIns tx `Set.isSubsetOf` utxoDomain (available w)
    return Wallet {
        walletUtxo    = walletUtxo
      , walletPending = Set.insert tx walletPending
      }

{-------------------------------------------------------------------------------
  Auxiliary operations
-------------------------------------------------------------------------------}

txIns :: (Hash h a, Foldable f) => f (Transaction h a) -> Set (Input h a)
txIns = Set.unions . map trIns . Fold.toList

txOuts :: (Hash h a, Foldable f) => f (Transaction h a) -> Utxo h a
txOuts = utxoUnions . map trUtxo . Fold.toList

available :: Hash h a => Wallet h a -> Utxo h a
available Wallet{..} = walletUtxo `utxoRemove` txIns walletPending

change :: Hash h a => Ours a -> Wallet h a -> Utxo h a
change ours Wallet{..} = filterUtxo ours (txOuts walletPending)

total :: Hash h a => Ours a -> Wallet h a -> Utxo h a
total ours w = available w `utxoUnion` change ours w

balance :: Utxo h a -> Value
balance = sum . map outVal . Map.elems . utxoToMap

updateUtxo :: forall h a. Hash h a
           => Ours a -> Block h a -> Utxo h a -> Utxo h a
updateUtxo ours b = remSpent . addNew
  where
    addNew, remSpent :: Utxo h a -> Utxo h a
    addNew   = (`utxoUnion`  filterUtxo ours (txOuts b))
    remSpent = (`utxoRemove`                  txIns  b)

updatePending :: forall h a. Hash h a => Block h a -> Pending h a -> Pending h a
updatePending b = Set.filter $ \t -> disjoint (trIns t) (txIns b)

filterUtxo :: Ours a -> Utxo h a -> Utxo h a
filterUtxo ours = utxoFilterByAddr (isJust . ours)

{-------------------------------------------------------------------------------
  Invariants
-------------------------------------------------------------------------------}

-- | Inductive definition of a wallet
--
-- TODO: We should generate random 'InductiveWallet's and then verify the
-- invariants.
data InductiveWallet h a =
    WalletEmpty
  | ApplyBlock (Block       h a) (InductiveWallet h a)
  | NewPending (Transaction h a) (InductiveWallet h a)

type Invariant h a = InductiveWallet h a -> Bool

-- | Check if a property is invariant for all wallet constructions
invariant :: forall h a. (Hash h a, Ord a)
          => Ours a -> (Wallet h a -> Bool) -> Invariant h a
invariant ours p = isJust . go
  where
    go :: InductiveWallet h a -> Maybe (Wallet h a)
    go WalletEmpty      = verify walletEmpty
    go (ApplyBlock b w) = go w >>= verify . applyBlock ours b
    go (NewPending t w) = go w >>= \w' ->
                          -- If pending not ours, just ignore
                          maybe (Just w') verify (newPending t w')

    verify :: Wallet h a -> Maybe (Wallet h a)
    verify w = guard (p w) >> return w

pendingInUtxo :: (Hash h a, Ord a) => Ours a -> Invariant h a
pendingInUtxo ours = invariant ours $ \Wallet{..} ->
    txIns walletPending `Set.isSubsetOf` utxoDomain walletUtxo

utxoIsOurs :: (Hash h a, Ord a) => Ours a -> Invariant h a
utxoIsOurs ours = invariant ours $ \Wallet{..} ->
    all (isJust . ours . outAddr) (utxoRange walletUtxo)

changeNotAvailable :: (Hash h a, Ord a) => Ours a -> Invariant h a
changeNotAvailable ours = invariant ours $ \w ->
    utxoDomain (change ours w) `disjoint` utxoDomain (available w)

changeNotInUtxo :: (Hash h a, Ord a) => Ours a -> Invariant h a
changeNotInUtxo ours = invariant ours $ \w ->
    utxoDomain (change ours w) `disjoint` utxoDomain (walletUtxo w)

changeAvailable :: (Hash h a, Ord a) => Ours a -> Invariant h a
changeAvailable ours = invariant ours $ \w ->
    change ours w `utxoUnion` available w == total ours w

balanceChangeAvailable :: (Hash h a, Ord a) => Ours a -> Invariant h a
balanceChangeAvailable ours = invariant ours $ \w ->
    balance (change ours w) + balance (available w) == balance (total ours w)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Check that two sets are disjoint
--
-- This is available out of the box from containters >= 0.5.11
disjoint :: Ord a => Set a -> Set a -> Bool
disjoint a b = Set.null (a `Set.intersection` b)
