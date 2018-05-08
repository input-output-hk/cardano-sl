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

  -- * Domain-specific errors
  , TxMetaStorageError (..)
  , InvariantViolation (..)

  -- * Deep & shallow equality
  , deepEq
  , shallowEq
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Text.Buildable (build)
import           Formatting (bprint, shown, (%))
import qualified Formatting as F
import           Pos.Crypto (shortHashF)
import           Serokell.Util.Text (listJsonIndent, mapBuilder)
import           Test.QuickCheck (Arbitrary (..), Gen)

import           Pos.Arbitrary.Core ()
import qualified Pos.Core as Core

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

-- | Deep equality for two 'TxMeta': two 'TxMeta' are equal if they have
-- exactly the same data, and inputs & outputs needs to appear in exactly
-- the same order.
deepEq :: TxMeta -> TxMeta -> Bool
deepEq t1 t2 =
    and [ t1 ^. txMetaId == t2 ^. txMetaId
        , t1 ^. txMetaAmount == t2 ^. txMetaAmount
        , t1 ^. txMetaInputs  == t2 ^. txMetaInputs
        , t1 ^. txMetaOutputs == t2 ^. txMetaOutputs
        , t1 ^. txMetaCreationAt == t2 ^. txMetaCreationAt
        , t1 ^. txMetaIsLocal == t2 ^. txMetaIsLocal
        , t1 ^. txMetaIsOutgoing == t2 ^. txMetaIsOutgoing
        ]

-- | Shallow equality for two 'TxMeta': two 'TxMeta' are equal if they have
-- the same data, even if in different order.
-- NOTE: This 'Eq' check might be slightly expensive as it's logaritmic in the
-- number of inputs & outputs, as it requires sorting.
shallowEq :: TxMeta -> TxMeta -> Bool
shallowEq t1 t2 =
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
      | UndisputableLookupFailed Core.TxId
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

instance Arbitrary TxMeta where
    arbitrary = TxMeta <$> arbitrary
                       <*> arbitrary
                       <*> uniqueElements
                       <*> uniqueElements
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary

-- | Generates 'NonEmpty' collections which do not contain duplicates.
-- Limit the size to ~10 elements
uniqueElements :: Gen (NonEmpty (Core.Address, Core.Coin))
uniqueElements = do
    (e :| es) <- arbitrary
    return (e :| (List.take 1 $ List.filter (/= e) (List.nub es)))

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
