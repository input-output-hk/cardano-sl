{-# LANGUAGE ViewPatterns #-}

-- | Pending transactions
--
-- Intended for qualified import:
--
-- > import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending)
-- > import qualified Cardano.Wallet.Kernel.DB.Spec.Pending as Pending
module Cardano.Wallet.Kernel.DB.Spec.Pending (
    Pending -- opaque
    -- * Basic combinators
  , null
  , lookup
  , member
  , empty
  , singleton
  , insert
  , delete
  , union
  , unions
  , isSubsetOf
  , disjoint
  , (\\)
    -- * Conversions
  , fromTransactions
  , toList
  , transactions
  , transactionIds
    -- * Custom operations
  , txIns
  , txOuts
  , change
  , removeInputs
  , PendingDiff
  , liftDeltaPending
  , liftStepPending
  ) where

import           Universum hiding (empty, null, toList)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Set as Set
import           Formatting (bprint, (%))
import qualified Formatting.Buildable
import           Serokell.Util (mapJson)

import qualified Pos.Chain.Txp as Core
import qualified Pos.Core as Core
import           Pos.Crypto.Hashing (hash)

import           Cardano.Wallet.Kernel.DB.InDb
import qualified Cardano.Wallet.Kernel.Util as Util
import qualified Cardano.Wallet.Kernel.Util.Core as Core

{-------------------------------------------------------------------------------
  Pending transactions
-------------------------------------------------------------------------------}

-- | Internal type: the underlying 'Map'
type UnderlyingMap = Map Core.TxId Core.TxAux

-- | Pending transactions
newtype Pending = Pending (InDb UnderlyingMap) deriving Eq

deriveSafeCopy 1 'base ''Pending

{-------------------------------------------------------------------------------
  Basic combinators
-------------------------------------------------------------------------------}

null :: Pending -> Bool
null = Map.null . toMap

lookup :: Core.TxId -> Pending -> Maybe Core.TxAux
lookup txId = Map.lookup txId . toMap

member :: Core.TxId -> Pending -> Bool
member txid = isJust . (lookup txid)

-- | Returns a new, empty 'Pending' set.
empty :: Pending
empty = fromMap Map.empty

-- | Returns a new, empty 'Pending' set.
singleton :: Core.TxAux -> Pending
singleton txAux = fromMap $ Map.singleton (Core.txAuxId txAux) txAux

insert :: Core.TxAux -> Pending -> Pending
insert tx = liftMap $ Map.insert (hash (Core.taTx tx)) tx

-- | Remove the specified transactions from the pending set
--
-- Do not confuse with 'removeInputs'.
delete :: Set Core.TxId -> Pending -> Pending
delete ids = liftMap (`Util.withoutKeys` ids)

-- | Computes the union between two 'Pending' sets.
union :: Pending -> Pending -> Pending
union = liftMap2 Map.union

unions :: [Pending] -> Pending
unions = fromMap . Map.unions . map toMap

isSubsetOf :: Pending -> Pending -> Bool
(toMap -> a) `isSubsetOf` (toMap -> b) = a `Map.isSubmapOf` b

disjoint :: Pending -> Pending -> Bool
disjoint (toMap -> a) (toMap -> b) =
    Util.disjoint (Map.keysSet a) (Map.keysSet b)

(\\) :: Pending -> Pending -> Pending
(toMap -> a) \\ (toMap -> b) = fromMap $ a `Util.withoutKeys` Map.keysSet b

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

fromTransactions :: [Core.TxAux] -> Pending
fromTransactions = fromList . map (\tx -> (hash (Core.taTx tx), tx))

toList :: Pending -> [(Core.TxId, Core.TxAux)]
toList = Map.toList . toMap

transactions :: Pending -> [Core.TxAux]
transactions = map snd . toList

transactionIds :: Pending -> Set (Core.TxId)
transactionIds = Map.keysSet . toMap

{-------------------------------------------------------------------------------
  Custom operations
-------------------------------------------------------------------------------}

-- | All inputs spent by any of the pending transactions
txIns :: Pending -> Set Core.TxIn
txIns = Set.fromList
      . concatMap (NE.toList . Core._txInputs . Core.taTx)
      . Map.elems
      . toMap

-- | All outputs of the pending transactions
rawTxOuts :: Pending -> Core.Utxo
rawTxOuts = Core.utxoUnions . map (Core.txOuts . Core.taTx) . Map.elems . toMap

-- | All outputs of the pending transactions that are not spent by other
-- pending transactions
--
-- Implementation note: 'rawTxOuts' and 'txOuts' can be different only in the
-- presence of rollbacks; see section "Rollback -- Model" in the formal
-- specification.
txOuts :: Pending -> Core.Utxo
txOuts p = rawTxOuts p `Core.utxoRemoveInputs` txIns p

-- | Outputs in 'txOuts' that belong to the wallet
change :: (Core.Address -> Bool) -> Pending -> Core.Utxo
change p = Map.filter p' . txOuts
  where
    p' :: Core.TxOutAux -> Bool
    p' = p . Core.txOutAddress . Core.toaOut

-- | Remove any transactions that have any of the specified inputs
--
-- Returns the set of transactions that were removed from the pending set.
--
-- Do not confuse with 'delete'.
--
-- NOTE: If we want to distinguish between transactions that got evicted because
-- they were confirmed from transactions that got evicted because they were
-- invalid, we would need to return more detailed info here (CBR-367).
removeInputs :: Set Core.TxIn -> Pending -> (Pending, Set Core.TxId)
removeInputs usedInputs (toMap -> p) =
    let (pToKeep, pToEvict) = Map.partition shouldKeep p
    in (fromMap pToKeep, Map.keysSet pToEvict)
  where
    shouldKeep :: Core.TxAux -> Bool
    shouldKeep tx = Util.disjoint (Core.txIns tx) usedInputs

{-------------------------------------------------------------------------------
  Internal auxiliary

  These are not exported because they might break the invariant that the
  @TxId@s match the @TxAux@s.
-------------------------------------------------------------------------------}

fromList :: [(Core.TxId, Core.TxAux)] -> Pending
fromList = fromMap . Map.fromList

fromMap :: UnderlyingMap -> Pending
fromMap = Pending . InDb

toMap :: Pending -> UnderlyingMap
toMap (Pending (InDb p)) = p

liftMap :: (UnderlyingMap -> UnderlyingMap) -> Pending -> Pending
liftMap f (toMap -> p) = fromMap (f p)

liftMap2 :: (UnderlyingMap -> UnderlyingMap -> UnderlyingMap)
         -> Pending -> Pending -> Pending
liftMap2 f (toMap -> p) (toMap -> p') = fromMap (f p p')

{--------------------------------------------------------------------------------
  Compression
-------------------------------------------------------------------------------}

type PendingDiff = InDb (Map Core.TxId Core.TxAux, Set Core.TxId)

liftDeltaPending :: (UnderlyingMap -> UnderlyingMap -> (Map Core.TxId Core.TxAux, Set Core.TxId))
    -> Pending -> Pending -> PendingDiff
liftDeltaPending f (Pending (InDb m)) (Pending (InDb m')) = InDb (f m m')

liftStepPending :: (UnderlyingMap -> (Map Core.TxId Core.TxAux, Set Core.TxId) -> UnderlyingMap)
    -> Pending -> PendingDiff -> Pending
liftStepPending f (Pending (InDb m)) (InDb d) = Pending . InDb $ f m d

{-------------------------------------------------------------------------------
  Pretty printing
-------------------------------------------------------------------------------}

instance Buildable Pending where
    build (Pending (InDb p)) = bprint ("Pending " % mapJson) p
