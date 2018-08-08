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

import qualified Data.List.NonEmpty as NE
import           Formatting.Buildable (Buildable (..))

import           Pos.Chain.Block (MainBlock, gbBody, mbTxs, mbWitnesses)
import           Pos.Core.Txp (Tx, TxAux (..), TxIn (..), txInputs)

import           Formatting (bprint, (%))
import qualified Formatting as F

import           Cardano.Wallet.Kernel.ChainState
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Resolved
import qualified Cardano.Wallet.Kernel.Util.Core as Core

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

    deriving (Eq, Ord)

instance Buildable WalletId where
    build (WalletIdHdRnd rootId) =
        bprint ("WalletIdHdRnd " % F.build) rootId

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
    deriving (Eq, Ord)

instance Buildable AccountId where
    build (AccountIdHdRnd accountId) =
        bprint ("AccountIdHdRnd " % F.build) accountId

{-------------------------------------------------------------------------------
  Input resolution: raw types
-------------------------------------------------------------------------------}

-- | All resolved inputs of a transaction
type ResolvedTxInputs = NonEmpty ResolvedInput

-- | All resolved inputs of a block
type ResolvedBlockInputs = [ResolvedTxInputs]

-- | Signed transaction along with its resolved inputs
--
-- Constructor is marked as unsafe because the caller should make sure that
-- invariant 'invRawResolvedTx' holds.
data RawResolvedTx = UnsafeRawResolvedTx {
      rawResolvedTx       :: TxAux
    , rawResolvedTxInputs :: ResolvedTxInputs
    }

-- | Invariant for 'RawResolvedTx'
--
-- > number of inputs @==@ number of resolved inputs
invRawResolvedTx :: TxAux -> ResolvedTxInputs -> Bool
invRawResolvedTx txAux ins = length (taTx txAux ^. txInputs) == length ins

-- | Smart constructor for 'RawResolvedTx' that checks the invariant
mkRawResolvedTx :: TxAux -> ResolvedTxInputs -> RawResolvedTx
mkRawResolvedTx txAux ins =
    if invRawResolvedTx txAux ins
      then UnsafeRawResolvedTx txAux ins
      else error "mkRawResolvedTx: invariant violation"

-- | Signed block along with its resolved inputs
--
-- Constructor is marked unsafe because the caller should make sure that
-- invariant 'invRawResolvedBlock' holds.
data RawResolvedBlock = UnsafeRawResolvedBlock {
      -- | The underlying 'MainBlock'
      rawResolvedBlock       :: MainBlock

      -- | Resolved inputs
      --
      -- Working with these inputs is more convenient using a 'ResolvedBlock';
      -- see 'fromRawResolvedBlock'.
    , rawResolvedBlockInputs :: ResolvedBlockInputs
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
                   -> RawResolvedBlock
mkRawResolvedBlock block ins =
    if invRawResolvedBlock block ins
      then UnsafeRawResolvedBlock block ins
      else error "mkRawResolvedBlock: invariant violation"

{-------------------------------------------------------------------------------
  Construct derived types from raw types
-------------------------------------------------------------------------------}

fromRawResolvedTx :: RawResolvedTx -> ResolvedTx
fromRawResolvedTx rtx = ResolvedTx {
      _rtxInputs  = InDb $ NE.zip inps (rawResolvedTxInputs rtx)
    , _rtxOutputs = InDb $ Core.txOuts tx
    }
  where
    tx :: Tx
    tx = taTx (rawResolvedTx rtx)

    inps :: NonEmpty TxIn
    inps = tx ^. txInputs

fromRawResolvedBlock :: ChainBrief -> RawResolvedBlock -> ResolvedBlock
fromRawResolvedBlock brief rb = ResolvedBlock {
      _rbTxs   = zipWith aux (getBlockTxs b)
                             (rawResolvedBlockInputs rb)
    , _rbBrief = brief
    }
  where
    b = rawResolvedBlock rb

    -- Justification for the use of the unsafe constructor:
    -- The invariant for 'RawResolvedBlock' guarantees the invariant for the
    -- individual transactions.
    aux :: TxAux -> ResolvedTxInputs -> ResolvedTx
    aux txAux ins = fromRawResolvedTx $ UnsafeRawResolvedTx txAux ins

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getBlockTxs :: MainBlock -> [TxAux]
getBlockTxs b = zipWith TxAux (b ^. gbBody ^. mbTxs)
                              (b ^. gbBody ^. mbWitnesses)
