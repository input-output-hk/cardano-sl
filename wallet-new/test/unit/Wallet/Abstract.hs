{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Abstract definition of a wallet
module Wallet.Abstract (
    -- * Abstract definition of a wallet
    IsWallet(..)
  , Ours
  , Pending
    -- * Inductive wallet definition
  , Inductive(..)
  , interpret
    -- ** Invariants
  , Invariant
  , invariant
    -- ** Testing
  , walletInvariants
  , walletEquivalent
    -- * Auxiliary operations
  , balance
  , txIns
  , txOuts
  , updatePending
  , utxoRestrictToOurs
  ) where

import Universum
import qualified Data.Foldable as Fold
import qualified Data.Set      as Set
import qualified Data.Map      as Map

import UTxO.DSL
import UTxO.Crypto

{-------------------------------------------------------------------------------
  Wallet type class
-------------------------------------------------------------------------------}

-- | Check if an address is ours
type Ours a = a -> Maybe SomeKeyPair

-- | Pending transactions
type Pending h a = Set (Transaction h a)

-- | Abstract definition of a wallet
class (Hash h a, Ord a) => IsWallet w h a where
  pending    :: w h a -> Pending h a
  utxo       :: w h a -> Utxo h a
  ours       :: w h a -> Ours a
  applyBlock :: Block h a -> w h a -> w h a
  newPending :: Transaction h a -> w h a -> Maybe (w h a)

  -- Operations with default implementations

  availableBalance :: IsWallet w h a => w h a -> Value
  availableBalance = balance . available

  totalBalance :: IsWallet w h a => w h a -> Value
  totalBalance = balance . total

  available :: IsWallet w h a => w h a -> Utxo h a
  available w = utxoRemoveInputs (txIns (pending w)) (utxo w)

  change :: IsWallet w h a => w h a -> Utxo h a
  change w = utxoRestrictToOurs (ours w) (txOuts (pending w))

  total :: IsWallet w h a => w h a -> Utxo h a
  total w = available w `utxoUnion` change w

-- | Variation on 'newPending' which simply ignores any transactions
-- that do not belong to us
newPending' :: IsWallet w h a => Transaction h a -> w h a -> w h a
newPending' tx w = fromMaybe w $ newPending tx w

{-------------------------------------------------------------------------------
  Interlude: "functor" over different wallet types (internal use only)
-------------------------------------------------------------------------------}

data Wallets :: [(* -> *) -> * -> *] -> (* -> *) -> * -> * where
  One :: IsWallet w h a
      => w h a -> Wallets '[w] h a

  Two :: (IsWallet w h a, IsWallet w' h a)
      => w h a -> w' h a -> Wallets '[w,w'] h a

walletsMap :: (forall w. IsWallet w h a => w h a -> w h a)
           -> Wallets ws h a -> Wallets ws h a
walletsMap f (One w)    = One (f w)
walletsMap f (Two w w') = Two (f w) (f w')

{-------------------------------------------------------------------------------
  Inductive wallet definition
-------------------------------------------------------------------------------}

-- | Inductive definition of a wallet
--
-- TODO: We should generate random 'Inductive's and then verify the
-- invariants.
data Inductive h a =
    WalletEmpty
  | ApplyBlock (Block       h a) (Inductive h a)
  | NewPending (Transaction h a) (Inductive h a)

-- | Interpreter for 'Inductive'
--
-- Given (one or more) empty wallets, evaluate an 'Inductive' wallet, checking
-- the given property at each step.
interpret :: forall ws h a.
             Wallets ws h a                      -- ^ Empty wallet
          -> (Wallets ws h a -> Either Text ())  -- ^ Predicate to check
          -> Inductive h a -> Either Text (Wallets ws h a)
interpret es p = go
  where
    go :: Inductive h a -> Either Text (Wallets ws h a)
    go WalletEmpty      = verify es
    go (ApplyBlock b w) = go w >>= verify . walletsMap (applyBlock b)
    go (NewPending t w) = go w >>= verify . walletsMap (newPending' t)

    verify :: Wallets ws h a -> Either Text (Wallets ws h a)
    verify ws = p ws >> return ws

inductiveGen 
    :: Gen (Block h a)
    -> Gen (Transaction h a)
    -> Gen (Inductive h a)
inductiveGen mkBlock mkTransaction = sized go
  where 
    go n
        | n <= 1 = pure WalletEmpty
        | otherwise = do
            this <- frequency 
                [ (1, ApplyBlock <$> mkBlock)
                , (9, NewPending <$> mkTransaction)
                ]
            next <- go (n-1)
            pure (this next)

{-------------------------------------------------------------------------------
  Invariants
-------------------------------------------------------------------------------}

-- | Wallet invariant
--
-- A wallet invariant is a property that is preserved by the fundamental
-- wallet operations, as defined by the 'IsWallet' type class and the
-- definition of 'Inductive'.
--
-- In order to evaluate the inductive definition we need the empty wallet
-- to be passed as a starting point.
type Invariant h a = Inductive h a -> Either Text ()

-- | Lift a property of flat wallet values to an invariant over the wallet ops
invariant :: IsWallet w h a => Text -> w h a -> (w h a -> Bool) -> Invariant h a
invariant err e p = void . interpret (One e) p'
  where
    p' (One w) = if p w then Right () else Left err

{-------------------------------------------------------------------------------
  Specific invariants
-------------------------------------------------------------------------------}

walletInvariants :: IsWallet w h a => w h a -> Invariant h a
walletInvariants e w = sequence_ [
      pendingInUtxo          e w
    , utxoIsOurs             e w
    , changeNotAvailable     e w
    , changeNotInUtxo        e w
    , changeAvailable        e w
    , balanceChangeAvailable e w
    ]

pendingInUtxo :: IsWallet w h a => w h a -> Invariant h a
pendingInUtxo e = invariant "pendingInUtxo" e $ \w ->
    txIns (pending w) `Set.isSubsetOf` utxoDomain (utxo w)

utxoIsOurs :: IsWallet w h a => w h a -> Invariant h a
utxoIsOurs e = invariant "utxoIsOurs" e $ \w ->
    all (isJust . ours w . outAddr) (utxoRange (utxo w))

changeNotAvailable :: IsWallet w h a => w h a -> Invariant h a
changeNotAvailable e = invariant "changeNotAvailable" e $ \w ->
    utxoDomain (change w) `disjoint` utxoDomain (available w)

changeNotInUtxo :: IsWallet w h a => w h a -> Invariant h a
changeNotInUtxo e = invariant "changeNotInUtxo" e $ \w ->
    utxoDomain (change w) `disjoint` utxoDomain (utxo w)

changeAvailable :: IsWallet w h a => w h a -> Invariant h a
changeAvailable e = invariant "changeAvailable" e $ \w ->
    change w `utxoUnion` available w == total w

balanceChangeAvailable :: IsWallet w h a => w h a -> Invariant h a
balanceChangeAvailable e = invariant "balanceChangeAvailable" e $ \w ->
    balance (change w) + balance (available w) == balance (total w)

{-------------------------------------------------------------------------------
  Compare different wallet implementations
-------------------------------------------------------------------------------}

walletEquivalent :: forall w w' h a. (IsWallet w h a, IsWallet w' h a)
                 => w h a -> w' h a -> Invariant h a
walletEquivalent e e' = void . interpret (Two e e') p
  where
    p :: Wallets '[w,w'] h a -> Either Text ()
    p (Two w w') = sequence_ [
          cmp "pending"          pending
        , cmp "utxo"             utxo
        , cmp "availableBalance" availableBalance
        , cmp "totalBalance"     totalBalance
        , cmp "available"        available
        , cmp "change"           change
        , cmp "total"            total
        ]
      where
        cmp :: Eq b
            => Text
            -> (forall w''. IsWallet w'' h a => w'' h a -> b)
            -> Either Text ()
        cmp err f = if f w == f w' then Right () else Left err

{-------------------------------------------------------------------------------
  Auxiliary operations
-------------------------------------------------------------------------------}

balance :: Utxo h a -> Value
balance = sum . map outVal . Map.elems . utxoToMap

txIns :: (Hash h a, Foldable f) => f (Transaction h a) -> Set (Input h a)
txIns = Set.unions . map trIns . Fold.toList

txOuts :: (Hash h a, Foldable f) => f (Transaction h a) -> Utxo h a
txOuts = utxoUnions . map trUtxo . Fold.toList

updatePending :: forall h a. Hash h a => Block h a -> Pending h a -> Pending h a
updatePending b = Set.filter $ \t -> disjoint (trIns t) (txIns b)

utxoRestrictToOurs :: Ours a -> Utxo h a -> Utxo h a
utxoRestrictToOurs p = utxoRestrictToAddr (isJust . p)

{-------------------------------------------------------------------------------
  Util
-------------------------------------------------------------------------------}

-- | Check that two sets are disjoint
--
-- This is available out of the box from containters >= 0.5.11
disjoint :: Ord a => Set a -> Set a -> Bool
disjoint a b = Set.null (a `Set.intersection` b)
