{-# LANGUAGE GADTs #-}

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
  , txMetaWalletId
  , txMetaAccountIx

  -- * Transaction storage
  , MetaDBHandle (..)

  -- * Filtering and sorting primitives
  , AccountFops (..)
  , FilterOperation (..)
  , FilterOrdering (..)
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
  , txIdIsomorphic

  -- * Internals useful for testing
  , uniqueElements
  , quadF
  , PutReturn (..)
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text.Lazy.Builder as B
import           Formatting (bprint, shown, (%))
import qualified Formatting as F
import           Formatting.Buildable (build)
import           Pos.Crypto (shortHashF)
import           Serokell.Util.Text (listBuilderJSON, listJsonIndent,
                     mapBuilder)
import           Test.QuickCheck (Arbitrary (..), Gen, suchThat)

import qualified Pos.Chain.Txp as Txp
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
      _txMetaId         :: Txp.TxId

      -- | Total amount
    , _txMetaAmount     :: Core.Coin

      -- | Transaction inputs
    , _txMetaInputs     :: NonEmpty (Txp.TxId, Word32, Core.Address, Core.Coin)

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

      -- The Wallet that added this Tx.
    , _txMetaWalletId   :: Core.Address

      -- The account index that added this Tx
    , _txMetaAccountIx  :: Word32
    } deriving Show

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
        , t1 ^. txMetaWalletId == t2 ^. txMetaWalletId
        , t1 ^. txMetaAccountIx == t2 ^. txMetaAccountIx
        ]

-- | Lenient equality for two 'TxMeta': two 'TxMeta' are equal if they have
-- the same data, same outputs in the same order and same inputs even if in different order.
-- NOTE: This check might be slightly expensive as it's nlogn in the
-- number of inputs, as it requires sorting.
isomorphicTo :: TxMeta -> TxMeta -> Bool
isomorphicTo t1 t2 =
    and [ t1 ^. txMetaId == t2 ^. txMetaId
        , t1 ^. txMetaAmount == t2 ^. txMetaAmount
        , NonEmpty.sort (t1 ^. txMetaInputs)  == NonEmpty.sort (t2 ^. txMetaInputs)
        , t1 ^. txMetaOutputs == t2 ^. txMetaOutputs
        , t1 ^. txMetaCreationAt == t2 ^. txMetaCreationAt
        , t1 ^. txMetaIsLocal == t2 ^. txMetaIsLocal
        , t1 ^. txMetaIsOutgoing == t2 ^. txMetaIsOutgoing
        , t1 ^. txMetaWalletId == t2 ^. txMetaWalletId
        , t1 ^. txMetaAccountIx == t2 ^. txMetaAccountIx
        ]

-- This means TxMeta have same Inputs and TxId.
txIdIsomorphic :: TxMeta -> TxMeta -> Bool
txIdIsomorphic t1 t2 =
    and [ t1 ^. txMetaId == t2 ^. txMetaId
        , NonEmpty.sort (t1 ^. txMetaInputs)  == NonEmpty.sort (t2 ^. txMetaInputs)
        , t1 ^. txMetaOutputs == t2 ^. txMetaOutputs
        ]

type AccountIx = Word32
type WalletId = Core.Address
-- | Filter Operations on Accounts. This is hiererchical: you can`t have AccountIx without WalletId.
data AccountFops = Everything | AccountFops WalletId (Maybe AccountIx)

data InvariantViolation =
        TxIdInvariantViolated Txp.TxId
        -- ^ When attempting to insert a new 'MetaTx', the TxId
        -- identifying this transaction was already present in the storage,
        -- but with different values (i.e. different inputs/outputs etc)
        -- and this is effectively an invariant violation.
      | UndisputableLookupFailed Text
        -- ^ The db works in a try-catch style: it always first tries to
        -- insert data and if the PrimaryKey is already there, we catch the
        -- exception and do the lookup. This lookup should never fail, because
        -- the db is append only and if it`s found once, it should always
        -- be there.
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
                           " inputs = " % F.later tQuadBuilder %
                           " outputs = " % F.later mapBuilder %
                           " creationAt = " % F.build %
                           " isLocal = " % F.build %
                           " isOutgoing = " % F.build %
                           " walletId = " % F.build %
                           " accountIx = " % F.build
                          ) (txMeta ^. txMetaId)
                            (txMeta ^. txMetaAmount)
                            (txMeta ^. txMetaInputs)
                            (txMeta ^. txMetaOutputs)
                            (txMeta ^. txMetaCreationAt)
                            (txMeta ^. txMetaIsLocal)
                            (txMeta ^. txMetaIsOutgoing)
                            (txMeta ^. txMetaWalletId)
                            (txMeta ^. txMetaAccountIx)

tQuadBuilder
    :: (Traversable t, Buildable a, Buildable b, Buildable c, Buildable d)
    => t (a, b, c, d) -> B.Builder
tQuadBuilder = listBuilderJSON . fmap quadBuilder

quadBuilder
    :: (Buildable a, Buildable b, Buildable c, Buildable d)
    => (a, b, c, d) -> B.Builder
quadBuilder (a, b, c, d) = bprint ("(" % F.build % ", " % F.build % ", "
      % F.build % ", " % F.build % ")") a b c d

quadF :: (Buildable a, Buildable b, Buildable c, Buildable d) => F.Format r ((a,b,c,d) -> r)
quadF = F.later quadBuilder

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

data FilterOperation a =
    NoFilterOp
    -- ^ No filter operation provided
    | FilterByIndex a
    -- ^ Filter by index (e.g. equal to)
    | FilterByPredicate FilterOrdering a
    -- ^ Filter by predicate (e.g. lesser than, greater than, etc.)
    | FilterByRange a a
    -- ^ Filter by range, in the form [from,to]
    | FilterIn [a]
    deriving (Show, Eq)

data FilterOrdering =
      Equal
    | GreaterThan
    | GreaterThanEqual
    | LesserThan
    | LesserThanEqual
    deriving (Show, Eq, Enum, Bounded)

-- This is used mainly for testing and indicates, what happening
-- at the internals of SQlite during a putTxMetaT operation
-- @Tx@ means a new Tx was inserted
-- @Meta@ means the Tx was there but from a different Account, so a new TxMeta entry was created.
-- @No@ means the Tx was there from the same Account. This means nothing happens internally.
data PutReturn = Tx | Meta | No
    deriving (Show, Eq, Enum, Bounded)

instance Buildable PutReturn where
  build ret = bprint shown ret

-- | An opaque handle to the underlying storage, which can be easily instantiated
-- to a more concrete implementation like a Sqlite database, or even a pure
-- K-V store.
data MetaDBHandle = MetaDBHandle {
      closeMetaDB   :: IO ()
    , migrateMetaDB :: IO ()
    , clearMetaDB   :: IO ()
    , getTxMeta     :: Txp.TxId -> Core.Address -> Word32 -> IO (Maybe TxMeta)
    , putTxMeta     :: TxMeta -> IO ()
    , putTxMetaT    :: TxMeta -> IO PutReturn
    , getAllTxMetas :: IO [TxMeta]
    , getTxMetas    :: Offset -- Pagination: the starting offset of results.
                    -> Limit  -- An upper limit of the length of [TxMeta] returned.
                    -> AccountFops -- Filters on the Account. This may specidy an Account or a Wallet.
                    -> Maybe Core.Address -- Filters on the Addres.
                    -> FilterOperation Txp.TxId -- Filters on the TxId of the Tx.
                    -> FilterOperation Core.Timestamp -- Filters on the creation timestamp of the Tx.
                    -> Maybe Sorting -- Sorting of the results.
                    -> IO ([TxMeta], Maybe Int) -- the result in the form (results, totalEntries).
                                                -- totalEntries may be Nothing, because counting can
                                                -- be an expensive operation.
    }
