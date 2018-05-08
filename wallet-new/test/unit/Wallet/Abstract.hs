{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Abstract definition of a wallet
module Wallet.Abstract (
    -- * Abstract definition of a wallet
    Wallet(..)
  , Ours
  , Pending
  , WalletConstr
  , mkDefaultWallet
  , walletBoot
  , applyBlocks
    -- * Auxiliary operations
  , balance
  , txIns
  , txOuts
  , updateUtxo
  , updatePending
  , utxoRestrictToOurs
  ) where

import           Universum

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Buildable
import           Formatting (bprint)
import           Pos.Util.Chrono
import           Serokell.Util (listJson)

import           Util
import           UTxO.DSL

{-------------------------------------------------------------------------------
  Wallet type class
-------------------------------------------------------------------------------}

-- | Check if an address is ours
type Ours a = a -> Bool

-- | Pending transactions
type Pending h a = Set (Transaction h a)

-- | Abstract wallet interface
data Wallet h a = Wallet {
      -- MAIN API

      -- | Return the total balance of the wallet (see 'available')
      totalBalance     :: Value

      -- | Return the available balance of the wallet (see 'total')
    , availableBalance :: Value

      -- | Notify the wallet of a new block
    , applyBlock       :: Block h a -> Wallet h a

      -- | Submit a new transaction to be included in the blockchain
    , newPending       :: Transaction h a -> Maybe (Wallet h a)

      -- | Rollback
    , rollback         :: Wallet h a

      -- AUXILIARY API

      -- | Current set of pending transactions
    , pending          :: Pending h a

      -- | Wallet's current UTxO (ignoring pending transactions)
    , utxo             :: Utxo h a

      -- | Wallet's expected UTxO (if supported)
    , expectedUtxo     :: Utxo h a

      -- | Addresses that belong to the wallet
    , ours             :: Ours a

      -- | Change from the pending transactions
    , change           :: Utxo h a

      -- | Available UTxO
      --
      -- This is the UTxO with the inputs spent by the pending transactions
      -- removed.
    , available        :: Utxo h a

      -- | Total UTxO
      --
      -- This is the available UTxO where we add back the change from the
      -- pending transactions.
    , total            :: Utxo h a

      -- | Internal state for debugging purposes
    , dumpState        :: Text
    }

-- | Apply multiple blocks
applyBlocks :: Wallet h a -> Chain h a -> Wallet h a
applyBlocks w0 bs = foldl' applyBlock w0 bs

-- | Type of a wallet constructor
--
-- See <http://www.well-typed.com/blog/2018/03/oop-in-haskell/> for a
-- detailed discussion of the approach we take.
type WalletConstr h a st = (st -> Wallet h a) -> (st -> Wallet h a)

-- | Default wallet constructor
--
-- This does not pick any particular implementation, but provides some
-- default implementations of some of the wallet methods in terms of the
-- other methods.
mkDefaultWallet
  :: forall h a st. (Hash h a, Ord a, Buildable st)
  => Lens' st (Pending h a)
  -> WalletConstr h a st
mkDefaultWallet l self st = Wallet {
      -- Dealing with pending
      pending    = st ^. l
    , newPending = \tx -> do
          -- Here we check that the inputs to the given transaction are a
          -- subset of the available unspent transaction outputs that aren't
          -- part of a currently pending transaction.
          let x = trIns tx :: Set (Input h a)
              y = utxoDomain (available this) :: Set (Input h a)
          case x `Set.isSubsetOf` y of
             True  -> Just $ self (st & l %~ Set.insert tx)
             False -> Nothing
      -- UTxOs
    , available = utxoRemoveInputs (txIns (pending this)) (utxo this)
    , change    = utxoRestrictToOurs (ours this) (txOuts (pending this))
    , total     = available this `utxoUnion` change this
      -- Balance
    , availableBalance = balance $ available this
    , totalBalance     = balance $ total     this
      -- Debugging
    , dumpState  = pretty st
      -- Functions without a default
    , utxo         = error "mkDefaultWallet: no default for utxo"
    , expectedUtxo = error "mkDefaultWallet: no default for expectedUtxo"
    , ours         = error "mkDefaultWallet: no default for ours"
    , applyBlock   = error "mkDefaultWallet: no default for applyBlock"
    , rollback     = error "mkDefaultWallet: no default for rollback"
    }
  where
    this = self st

-- | Wallet state after the bootstrap transaction
walletBoot :: (Ours a -> Wallet h a) -- ^ Wallet constructor
           -> Ours a -> Transaction h a -> Wallet h a
walletBoot mkWallet p boot = applyBlock (mkWallet p) (OldestFirst [boot])

{-------------------------------------------------------------------------------
  Auxiliary operations
-------------------------------------------------------------------------------}

balance :: Utxo h a -> Value
balance = sum . map outVal . Map.elems . utxoToMap

txIns :: (Hash h a, Foldable f) => f (Transaction h a) -> Set (Input h a)
txIns = Set.unions . map trIns . Fold.toList

txOuts :: (Hash h a, Foldable f) => f (Transaction h a) -> Utxo h a
txOuts = utxoUnions . map trUtxo . Fold.toList

updateUtxo :: forall h a. Hash h a
           => Ours a -> Block h a -> Utxo h a -> Utxo h a
updateUtxo p b = remSpent . addNew
  where
    addNew, remSpent :: Utxo h a -> Utxo h a
    addNew   = utxoUnion (utxoRestrictToOurs p (txOuts b))
    remSpent = utxoRemoveInputs (txIns b)

updatePending :: forall h a. Hash h a => Block h a -> Pending h a -> Pending h a
updatePending b = Set.filter $ \t -> disjoint (trIns t) (txIns b)

utxoRestrictToOurs :: Ours a -> Utxo h a -> Utxo h a
utxoRestrictToOurs = utxoRestrictToAddr

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance (Hash h a, Buildable a) => Buildable (Pending h a) where
  build = bprint listJson . Set.toList
