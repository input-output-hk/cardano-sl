{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Kernel.Types (
    -- * Input resolution
    -- ** Raw types
    ResolvedTxInputs
  , ResolvedBlockInputs
  , RawResolvedTx(..)
  , invRawResolvedTx
  , mkRawResolvedTx
  , RawResolvedBlock(..)
  , invRawResolvedBlock
  , mkRawResolvedBlock
  -- ** Abstract Wallet/AccountIds
  , WalletId (..)
  , AccountId (..)
  , accountToWalletId
    -- ** From raw to derived types
  , fromRawResolvedTx
  , fromRawResolvedBlock
  ) where

import           Universum

import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import           Formatting.Buildable (Buildable (..))

import           Pos.Chain.Block (MainBlock, gbBody, mbTxs, mbWitnesses)
import           Pos.Chain.Txp (Tx, TxAux (..), TxId, TxIn (..), txInputs)
import qualified Pos.Core as Core

import           Formatting (bprint, (%))
import qualified Formatting as F

import           Cardano.Wallet.Kernel.DB.BlockContext
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Resolved
import qualified Cardano.Wallet.Kernel.Util.Core as Core

import           Test.QuickCheck (Arbitrary (..), oneof)

{-------------------------------------------------------------------------------
  Abstract WalletId and AccountId
-------------------------------------------------------------------------------}

-- | Wallet Id
--
-- A Wallet Id can take several forms, the simplest of which is a hash
-- of the Wallet public key
data WalletId =
    -- | HD wallet with randomly generated addresses
  WalletIdHdRnd HD.HdRootId

    {- potential future kinds of wallet IDs:
    -- | HD wallet with sequentially generated addresses
    | WalletIdHdSeq ...

    -- | External wallet (all crypto done off-site, like hardware wallets)
    | WalletIdExt ...
    -}

    deriving (Generic, Eq, Ord)

instance Aeson.ToJSON WalletId
instance Aeson.FromJSON WalletId

instance Buildable WalletId where
    build (WalletIdHdRnd rootId) =
        bprint ("WalletIdHdRnd " % F.build) rootId

instance Arbitrary WalletId where
    arbitrary = WalletIdHdRnd <$> arbitrary

accountToWalletId :: HD.HdAccountId -> WalletId
accountToWalletId accountId
    = WalletIdHdRnd $ accountId ^. HD.hdAccountIdParent

-- | Account Id
--
-- An Account Id can take several forms, the simplest of which is a
-- random-indexed, hardeded HD Account.
data AccountId =
    -- | HD wallet with randomly generated (hardened) index.
    AccountIdHdRnd HD.HdAccountId
    deriving (Generic, Eq, Ord)

instance Aeson.ToJSON AccountId
instance Aeson.FromJSON AccountId

instance Buildable AccountId where
    build (AccountIdHdRnd accountId) =
        bprint ("AccountIdHdRnd " % F.build) accountId

instance Arbitrary AccountId where
    arbitrary = oneof [ AccountIdHdRnd <$> arbitrary ]

{-------------------------------------------------------------------------------
  Input resolution: raw types
-------------------------------------------------------------------------------}

-- | All resolved inputs of a transaction
type ResolvedTxInputs = NonEmpty ResolvedInput

-- | All resolved inputs of a block
type ResolvedBlockInputs = [ResolvedTxInputs]

type RawMeta = Core.Timestamp

-- | Signed transaction along with its resolved inputs
--
-- Constructor is marked as unsafe because the caller should make sure that
-- invariant 'invRawResolvedTx' holds.
data RawResolvedTx = UnsafeRawResolvedTx {
      rawResolvedTx       :: !TxAux
    , rawResolvedTxInputs :: !ResolvedTxInputs
    , rawResolvedTxMeta   :: !RawMeta
    }

-- | Invariant for 'RawResolvedTx'
--
-- > number of inputs @==@ number of resolved inputs
invRawResolvedTx :: TxAux -> ResolvedTxInputs -> Bool
invRawResolvedTx txAux ins = length (taTx txAux ^. txInputs) == length ins

-- | Smart constructor for 'RawResolvedTx' that checks the invariant
mkRawResolvedTx :: Core.Timestamp -> TxAux -> ResolvedTxInputs -> RawResolvedTx
mkRawResolvedTx timestamp txAux ins =
    if invRawResolvedTx txAux ins
      then UnsafeRawResolvedTx txAux ins timestamp
      else error "mkRawResolvedTx: invariant violation"

-- | Signed block along with its resolved inputs
--
-- Constructor is marked unsafe because the caller should make sure that
-- invariant 'invRawResolvedBlock' holds.
data RawResolvedBlock = UnsafeRawResolvedBlock {
      -- | The underlying 'MainBlock'
      rawResolvedBlock       :: !MainBlock

      -- | Resolved inputs
      --
      -- Working with these inputs is more convenient using a 'ResolvedBlock';
      -- see 'fromRawResolvedBlock'.
    , rawResolvedBlockInputs :: !ResolvedBlockInputs

      -- | The creation time of this Block.

    , rawTimestamp           :: !Core.Timestamp

      -- | Block context
    , rawResolvedContext     :: !BlockContext
    }

-- | Invariant for 'RawResolvedBlock'
--
-- > number of transactions @==@ number of resolved transaction inputs
--
-- Moreover, 'invRawResolvedTx' should hold for each transaction.
invRawResolvedBlock :: MainBlock -> ResolvedBlockInputs -> Bool
invRawResolvedBlock block ins =
       length txs == length ins
    && all (uncurry invRawResolvedTx) (zip txs ins)
  where
    txs :: [TxAux]
    txs = getBlockTxs block

-- | Smart constructor for 'RawResolvedBlock' that checks the invariant
mkRawResolvedBlock :: MainBlock
                   -> ResolvedBlockInputs
                   -> Core.Timestamp
                   -> BlockContext
                   -> RawResolvedBlock
mkRawResolvedBlock block ins timestamp context =
    if invRawResolvedBlock block ins
      then UnsafeRawResolvedBlock block ins timestamp context
      else error "mkRawResolvedBlock: invariant violation"

{-------------------------------------------------------------------------------
  Construct derived types from raw types
-------------------------------------------------------------------------------}

fromRawResolvedTx :: RawResolvedTx -> ResolvedTx
fromRawResolvedTx UnsafeRawResolvedTx{..} = ResolvedTx {
      _rtxInputs  = InDb $ NE.zip inps rawResolvedTxInputs
    , _rtxOutputs = InDb $ Core.txOuts tx
    , _rtxMeta    = InDb $ (txId, rawResolvedTxMeta)
    }
  where
    tx :: Tx
    tx = taTx rawResolvedTx

    txId :: TxId
    txId = Core.txAuxId rawResolvedTx

    inps :: NonEmpty TxIn
    inps = tx ^. txInputs

fromRawResolvedBlock :: RawResolvedBlock -> ResolvedBlock
fromRawResolvedBlock UnsafeRawResolvedBlock{..} = ResolvedBlock {
      _rbTxs     = zipWith aux (getBlockTxs rawResolvedBlock)
                               rawResolvedBlockInputs
    , _rbContext = rawResolvedContext
    , _rbMeta    = rawTimestamp
    }
  where
    -- Justification for the use of the unsafe constructor:
    -- The invariant for 'RawResolvedBlock' guarantees the invariant for the
    -- individual transactions.
    aux :: TxAux -> ResolvedTxInputs -> ResolvedTx
    aux txAux ins = fromRawResolvedTx $ UnsafeRawResolvedTx txAux ins rawTimestamp

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getBlockTxs :: MainBlock -> [TxAux]
getBlockTxs b = zipWith TxAux (b ^. gbBody ^. mbTxs)
                              (b ^. gbBody ^. mbWitnesses)
