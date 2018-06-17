-- | Transaction metadata conform the wallet specification
module Cardano.Wallet.Kernel.DB.TxMeta.Types (
    -- * Transaction metadata
    TxMeta(..)
    -- ** Lenses
  , txMetaId
  , txMetaAmount
  , txMetaInputs
  , txMetaOutputs
  , txMetaCreationAt
  , txMetaIsLocal
  , txMetaIsOutgoing

  -- * Transaction storage
  , MetaDBHandle (..)

  -- * Filtering and sorting primitives
  , Limit (..)
  , Offset (..)
  , Sorting (..)
  , SortCriteria (..)
  , SortDirection (..)

  -- * Domain-specific errors
  , TxMetaStorageError (..)
  , InvariantViolation (..)

  -- * Strict & lenient equalities
  , exactlyEqualTo
  , isomorphicTo

  -- * Internals useful for testing
  , uniqueElements
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import           Data.Text.Buildable (build)
import           Formatting (bprint, shown, (%))
import qualified Formatting as F
import           Pos.Crypto (shortHashF)
import           Serokell.Util.Text (listJsonIndent, mapBuilder)
import           Test.QuickCheck (Arbitrary (..), Gen, suchThat)

import qualified Pos.Core as Core

import           Test.Pos.Core.Arbitrary ()

{-------------------------------------------------------------------------------
  Transaction metadata
-------------------------------------------------------------------------------}

-- | Transaction metadata

--
-- NOTE: This does /not/ live in the acid-state database (and consequently
-- does not need a 'SafeCopy' instance), because this will grow without bound.
data TxMeta = TxMeta {
      -- | Transaction ID
      _txMetaId         :: Core.TxId

      -- | Total amount
      --
      -- TODO: What does this mean?
    , _txMetaAmount     :: Core.Coin

      -- | Transaction inputs
    , _txMetaInputs     :: NonEmpty (Core.Address, Core.Coin)

      -- | Transaction outputs
    , _txMetaOutputs    :: NonEmpty (Core.Address, Core.Coin)

      -- | Transaction creation time
    , _txMetaCreationAt :: Core.Timestamp

      -- | Is this a local transaction?
      --
      -- A transaction is local when /all/ of its inputs and outputs are
      -- to and from addresses owned by this wallet.
    , _txMetaIsLocal    :: Bool

      -- | Is this an outgoing transaction?
      --
      -- A transaction is outgoing when it decreases the wallet's balance.
    , _txMetaIsOutgoing :: Bool
    }

makeLenses ''TxMeta

-- | Strict equality for two 'TxMeta': two 'TxMeta' are equal if they have
-- exactly the same data, and inputs & outputs needs to appear in exactly
-- the same order.
exactlyEqualTo :: TxMeta -> TxMeta -> Bool
exactlyEqualTo t1 t2 =
    and [ t1 ^. txMetaId == t2 ^. txMetaId
        , t1 ^. txMetaAmount == t2 ^. txMetaAmount
        , t1 ^. txMetaInputs  == t2 ^. txMetaInputs
        , t1 ^. txMetaOutputs == t2 ^. txMetaOutputs
        , t1 ^. txMetaCreationAt == t2 ^. txMetaCreationAt
        , t1 ^. txMetaIsLocal == t2 ^. txMetaIsLocal
        , t1 ^. txMetaIsOutgoing == t2 ^. txMetaIsOutgoing
        ]

-- | Lenient equality for two 'TxMeta': two 'TxMeta' are equal if they have
-- the same data, even if in different order.
-- NOTE: This check might be slightly expensive as it's logaritmic in the
-- number of inputs & outputs, as it requires sorting.
isomorphicTo :: TxMeta -> TxMeta -> Bool
isomorphicTo t1 t2 =
    and [ t1 ^. txMetaId == t2 ^. txMetaId
        , t1 ^. txMetaAmount == t2 ^. txMetaAmount
        , NonEmpty.sort (t1 ^. txMetaInputs)  == NonEmpty.sort (t2 ^. txMetaInputs)
        , NonEmpty.sort (t1 ^. txMetaOutputs) == NonEmpty.sort (t2 ^. txMetaOutputs)
        , t1 ^. txMetaCreationAt == t2 ^. txMetaCreationAt
        , t1 ^. txMetaIsLocal == t2 ^. txMetaIsLocal
        , t1 ^. txMetaIsOutgoing == t2 ^. txMetaIsOutgoing
        ]


data InvariantViolation =
        DuplicatedTransactionWithDifferentHash Core.TxId
        -- ^ When attempting to insert a new 'MetaTx', the 'Core.TxId'
        -- identifying this transaction was already present in the storage,
        -- but when computing the 'Hash' of two 'TxMeta', these values were not
        -- the same, meaning somebody is trying to re-insert the same 'Tx' in
        -- the storage with different values (i.e. different inputs/outputs etc)
        -- and this is effectively an invariant violation.
      | DuplicatedInputIn  Core.TxId
      | DuplicatedOutputIn Core.TxId
      | UndisputableLookupFailed Text Core.TxId
        -- ^ When looking up a transaction which the storage claims to be
        -- already present as a duplicate, such lookup failed. This is an
        -- invariant violation because a 'TxMeta' storage is append-only,
        -- therefore the data cannot possibly be evicted, and should be there
        -- by definition (or we wouldn't get a duplicate collision in the
        -- first place).
      deriving Show

-- | A domain-specific collection of things which might go wrong when
-- storing & retrieving 'TxMeta' from a persistent storage.
data TxMetaStorageError =
      InvariantViolated InvariantViolation
    -- ^ One of the invariant was violated.
    | StorageFailure SomeException
    -- ^ The underlying storage failed to fulfill the request.
    deriving Show

instance Exception TxMetaStorageError

instance Buildable TxMetaStorageError where
    build storageErr = bprint shown storageErr

-- | Generates 'NonEmpty' collections which do not contain duplicates.
-- Limit the size to @size@ elements.
uniqueElements :: (Arbitrary a, Ord a) => Int -> Gen (NonEmpty a)
uniqueElements size = do
    noDupes <- suchThat arbitrary (\s -> length s >= size)
    let (e, es) = Set.deleteFindMax noDupes
    return $ e :| List.take size (Set.toList es)

instance Buildable TxMeta where
    build txMeta = bprint (" id = "%shortHashF%
                           " amount = " % F.build %
                           " inputs = " % F.later mapBuilder %
                           " outputs = " % F.later mapBuilder %
                           " creationAt = " % F.build %
                           " isLocal = " % F.build %
                           " isOutgoing = " % F.build
                          ) (txMeta ^. txMetaId)
                            (txMeta ^. txMetaAmount)
                            (txMeta ^. txMetaInputs)
                            (txMeta ^. txMetaOutputs)
                            (txMeta ^. txMetaCreationAt)
                            (txMeta ^. txMetaIsLocal)
                            (txMeta ^. txMetaIsOutgoing)

instance Buildable [TxMeta] where
    build txMeta = bprint ("TxMetas: "%listJsonIndent 4) txMeta


-- | Basic filtering & sorting types.

newtype Offset = Offset { getOffset :: Integer }

newtype Limit  = Limit  { getLimit  :: Integer }

data SortDirection =
      Ascending
    | Descending

data Sorting = Sorting {
      sbCriteria  :: SortCriteria
    , sbDirection :: SortDirection
    }

data SortCriteria =
      SortByCreationAt
    -- ^ Sort by the creation time of this 'Kernel.TxMeta'.
    | SortByAmount
    -- ^ Sort the 'TxMeta' by the amount of money they hold.

-- | An opaque handle to the underlying storage, which can be easily instantiated
-- to a more concrete implementation like a Sqlite database, or even a pure
-- K-V store.
data MetaDBHandle = MetaDBHandle {
      closeMetaDB   :: IO ()
    , migrateMetaDB :: IO ()
    , getTxMeta     :: Core.TxId -> IO (Maybe TxMeta)
    , putTxMeta     :: TxMeta -> IO ()
    , getTxMetas    :: Offset -> Limit -> Maybe Sorting -> IO [TxMeta]
    }
