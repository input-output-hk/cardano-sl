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
  , txUtxo
  ) where

import           Universum

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Word (Word32)
import           Formatting.Buildable (Buildable (..))

import           Pos.Core (GenesisBlock, MainBlock, Tx, TxAux (..), TxId,
                     TxIn (..), TxOut, TxOutAux (..), gbBody, mainBlockSlot,
                     mbTxs, mbWitnesses, txInputs, txOutputs)
import           Pos.Crypto.Hashing (hash)
import           Pos.Txp (Utxo)
import           Serokell.Util (enumerate)

import           Formatting (bprint, (%))
import qualified Formatting as F

import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Resolved

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
-- If this block sits directly after an epoch boundary, it might additionally
-- have an attached epoch boundary block which should directly proceed it in the
-- chain . This is becuase the DSL contains no notion of epoch boundaries.
--
-- Constructor is marked unsafe because the caller should make sure that
-- invariant 'invRawResolvedBlock' holds.
data RawResolvedBlock = UnsafeRawResolvedBlock {
      rawResolvedBlock       :: MainBlock
    , rawResolvedBlockEBB    :: Maybe GenesisBlock
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
mkRawResolvedBlock :: MainBlock -> Maybe GenesisBlock -> ResolvedBlockInputs -> RawResolvedBlock
mkRawResolvedBlock block mebb  ins =
    if invRawResolvedBlock block ins
      then UnsafeRawResolvedBlock block mebb ins
      else error "mkRawResolvedBlock: invariant violation"

{-------------------------------------------------------------------------------
  Construct derived types from raw types
-------------------------------------------------------------------------------}

fromRawResolvedTx :: RawResolvedTx -> ResolvedTx
fromRawResolvedTx rtx = ResolvedTx {
      _rtxInputs  = InDb $ NE.zip inps (rawResolvedTxInputs rtx)
    , _rtxOutputs = InDb $ txUtxo_ tx txId
    }
  where
    tx :: Tx
    tx = taTx (rawResolvedTx rtx)

    txId = hash tx

    inps :: NonEmpty TxIn
    inps = tx ^. txInputs

txUtxo_ :: Tx -> TxId -> Utxo
txUtxo_ tx txId = Map.fromList $
                      map (toTxInOut txId) (outs tx)

txUtxo :: Tx -> Utxo
txUtxo tx = txUtxo_ tx txId
          where txId = hash tx

outs :: Tx -> [(Word32, TxOut)]
outs tx = enumerate $ toList $ tx ^. txOutputs

toTxInOut :: TxId -> (Word32, TxOut) -> (TxIn, TxOutAux)
toTxInOut txId (idx, out) = (TxInUtxo txId idx, TxOutAux out)

fromRawResolvedBlock :: RawResolvedBlock -> ResolvedBlock
fromRawResolvedBlock rb = ResolvedBlock {
      _rbTxs  = zipWith aux (getBlockTxs b)
                            (rawResolvedBlockInputs rb)

    , _rbSlot = InDb (b ^. mainBlockSlot)
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
