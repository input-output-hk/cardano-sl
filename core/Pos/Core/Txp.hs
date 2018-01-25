-- | Core types of Txp component, i. e. types which actually form
-- block or are used by other components.

module Pos.Core.Txp
       ( TxId

       -- * Witness
       , TxInWitness (..)
       , TxWitness
       , TxSigData (..)
       , TxSig

       -- * Tx parts
       , TxIn (..)
       , TxOut (..)
       , _TxOut
       , TxOutAux (..)
       , TxAttributes
       , isTxInUnknown

       -- * Tx
       , Tx (..)
       , TxAux (..)
       , mkTx
       , txInputs
       , txOutputs
       , txAttributes
       , txF
       , txaF

       -- * Payload and proof
       , TxProof (..)
       , mkTxProof
       , TxPayload (..)
       , mkTxPayload
       , txpTxs
       , txpWitnesses

       -- * Undo
       , TxUndo
       , TxpUndo
       ) where

import           Universum

import           Control.Lens (makeLenses, makePrisms)
import           Data.Hashable (Hashable)
import qualified Data.Text.Buildable as Buildable
import           Data.Vector (Vector)
import           Fmt (genericF)
import           Formatting (Format, bprint, build, builder, int, later, sformat, (%))
import           Serokell.Util.Base16 (base16F)
import           Serokell.Util.Text (listJson, listJsonIndent)
import           Serokell.Util.Verify (VerificationRes (..), verResSingleF, verifyGeneric)

import           Pos.Binary.Class (Bi)
import           Pos.Binary.Core.Address ()
import           Pos.Binary.Crypto ()
import           Pos.Core.Common (Address (..), Coin, Script, addressHash, coinF, mkCoin)
import           Pos.Crypto (Hash, PublicKey, RedeemPublicKey, RedeemSignature, Signature, hash,
                             shortHashF)
import           Pos.Data.Attributes (Attributes, areAttributesKnown)
import           Pos.Merkle (MerkleRoot, MerkleTree, mkMerkleTree, mtRoot)

-- | Represents transaction identifier as 'Hash' of 'Tx'.
type TxId = Hash Tx

----------------------------------------------------------------------------
-- Witness
----------------------------------------------------------------------------

-- | Data that is being signed when creating a TxSig.
data TxSigData = TxSigData
    { -- | Transaction that we're signing
      txSigTxHash      :: !(Hash Tx)
    }
    deriving (Eq, Show, Generic, Typeable)

-- | 'Signature' of addrId.
type TxSig = Signature TxSigData

-- | A witness for a single input.
data TxInWitness
    = PkWitness { twKey :: !PublicKey
                , twSig :: !TxSig }
    | ScriptWitness { twValidator :: !Script
                    , twRedeemer  :: !Script }
    | RedeemWitness { twRedeemKey :: !RedeemPublicKey
                    , twRedeemSig :: !(RedeemSignature TxSigData) }
    | UnknownWitnessType !Word8 !ByteString
    deriving (Eq, Show, Generic, Typeable)

instance Hashable TxInWitness

instance Buildable TxInWitness where
    build (PkWitness key sig) =
        bprint ("PkWitness: key = "%build%", key hash = "%shortHashF%
                ", sig = "%build) key (addressHash key) sig
    build (ScriptWitness val red) =
        bprint ("ScriptWitness: "%
                "validator hash = "%shortHashF%", "%
                "redeemer hash = "%shortHashF) (hash val) (hash red)
    build (RedeemWitness key sig) =
        bprint ("PkWitness: key = "%build%", sig = "%build) key sig
    build (UnknownWitnessType t bs) =
        bprint ("UnknownWitnessType "%build%" "%base16F) t bs

instance NFData TxInWitness

-- | A witness is a proof that a transaction is allowed to spend the funds it
-- spends (by providing signatures, redeeming scripts, etc). A separate proof
-- is provided for each input.
type TxWitness = Vector TxInWitness

----------------------------------------------------------------------------
-- Tx parts
----------------------------------------------------------------------------

-- | Transaction arbitrary input.
data TxIn
    = TxInUtxo
    { -- | Which transaction's output is used
      txInHash  :: !TxId
      -- | Index of the output in transaction's outputs
    , txInIndex :: !Word32
    }
    | TxInUnknown !Word8 !ByteString
    deriving (Eq, Ord, Generic, Show, Typeable)

instance Hashable TxIn

instance Buildable TxIn where
    build TxInUtxo {..}        = bprint ("TxInUtxo "%shortHashF%" #"%int) txInHash txInIndex
    build (TxInUnknown tag bs) = bprint ("TxInUnknown "%int%" "%base16F) tag bs

instance NFData TxIn

isTxInUnknown :: TxIn -> Bool
isTxInUnknown (TxInUnknown _ _) = True
isTxInUnknown _                 = False

-- | Transaction output.
data TxOut = TxOut
    { txOutAddress :: !Address
    , txOutValue   :: !Coin
    } deriving (Eq, Ord, Generic, Show, Typeable)

instance Hashable TxOut

instance Buildable TxOut where
    build TxOut {..} =
        bprint ("TxOut "%coinF%" -> "%build) txOutValue txOutAddress

instance NFData TxOut

-- | Transaction output and auxilary data corresponding to it.
-- [CSL-366] Add more data.
data TxOutAux = TxOutAux
    { toaOut   :: !TxOut
    -- ^ Tx output
    } deriving (Generic, Show, Eq, Ord)

instance Buildable TxOutAux where
    build (TxOutAux out) = bprint ("{txout = "%build%"}") out

instance NFData TxOutAux

-- | Represents transaction attributes: map from 1-byte integer to
-- arbitrary-type value. To be used for extending transaction with new
-- fields via softfork.
type TxAttributes = Attributes ()

----------------------------------------------------------------------------
-- Tx
----------------------------------------------------------------------------

-- | Transaction.
--
-- NB: transaction witnesses are stored separately.
data Tx = UnsafeTx
    { _txInputs     :: !(NonEmpty TxIn)  -- ^ Inputs of transaction.
    , _txOutputs    :: !(NonEmpty TxOut) -- ^ Outputs of transaction.
    , _txAttributes :: !TxAttributes     -- ^ Attributes of transaction
    } deriving (Eq, Ord, Generic, Show, Typeable)

makeLenses ''Tx

-- | Transaction + auxiliary data
data TxAux = TxAux
    { taTx      :: !Tx
    , taWitness :: !TxWitness
    } deriving (Generic, Show, Eq)

instance Hashable Tx

instance Bi Tx => Buildable Tx where
    build tx@(UnsafeTx{..}) =
        bprint
            ("Tx "%build%
             " with inputs "%listJson%", outputs: "%listJson % builder)
            (hash tx) _txInputs _txOutputs attrsBuilder
      where
        attrs = _txAttributes
        attrsBuilder | areAttributesKnown attrs = mempty
                     | otherwise = bprint (", attributes: "%build) attrs

instance NFData Tx

-- | Specialized formatter for 'Tx'.
txF :: Bi Tx => Format r (Tx -> r)
txF = build

-- | Specialized formatter for 'TxAux'.
txaF :: Bi Tx => Format r (TxAux -> r)
txaF = later $ \(TxAux tx w) ->
    bprint (build%"\n"%"witnesses: "%listJsonIndent 4) tx w

instance Bi Tx => Buildable TxAux where
    build = bprint txaF

-- | Create valid Tx or fail.
-- Verify inputs and outputs are non empty; have enough coins.
mkTx :: NonEmpty TxIn -> NonEmpty TxOut -> TxAttributes -> Either Text Tx
mkTx inputs outputs attrs =
    case verRes of
        VerSuccess -> Right $ UnsafeTx inputs outputs attrs
        failure    -> Left $ sformat verResSingleF failure
  where
    verRes =
        verifyGeneric $
        concat $ zipWith outputPredicates [0 ..] $ toList outputs
    outputPredicates (i :: Word) TxOut {..} =
        [ ( txOutValue > mkCoin 0
          , sformat
                ("output #"%int%" has non-positive value: "%coinF)
                i txOutValue)
        ]

----------------------------------------------------------------------------
-- Payload and proof
----------------------------------------------------------------------------

data TxProof = TxProof
    { txpNumber        :: !Word32
    , txpRoot          :: !(MerkleRoot Tx)
    , txpWitnessesHash :: !(Hash [TxWitness])
    } deriving (Show, Eq, Generic)

instance Buildable TxProof where
    build = genericF

instance NFData TxProof

-- | Construct 'TxProof' which proves given 'TxPayload'.
mkTxProof :: Bi TxInWitness => TxPayload -> TxProof
mkTxProof UnsafeTxPayload {..} =
    TxProof
    { txpNumber = fromIntegral (length _txpTxs)
    , txpRoot = mtRoot _txpTxs
    , txpWitnessesHash = hash _txpWitnesses
    }

-- | Payload of Txp component which is part of main block. Constructor
-- is unsafe, because it lets one create invalid payload, for example
-- with different number of transactions and witnesses.
data TxPayload = UnsafeTxPayload
    { -- | Transactions are the main payload.
      _txpTxs       :: !(MerkleTree Tx)
    , -- | Witnesses for each transaction. The length of this field is
      -- checked during deserialisation; we can't put witnesses into the same
      -- Merkle tree with transactions, as the whole point of SegWit is to
      -- separate transactions and witnesses.
      --
      -- TODO: should they be put into a separate Merkle tree or left as
      -- a list?
      _txpWitnesses :: ![TxWitness]
    } deriving (Show, Eq, Generic)

instance NFData TxPayload

makeLenses ''TxPayload

-- | Smart constructor of 'TxPayload' which ensures that invariants of 'TxPayload' hold.
--
-- Currently there is only one invariant:
-- • number of txs must be same as number of witnesses.
mkTxPayload :: (Bi Tx) => [TxAux] -> TxPayload
mkTxPayload txws = do
    UnsafeTxPayload {..}
  where
    (txs, _txpWitnesses) =
            unzip . map (liftA2 (,) taTx taWitness) $ txws
    _txpTxs = mkMerkleTree txs

----------------------------------------------------------------------------
-- Undo
----------------------------------------------------------------------------

-- | Particular undo needed for transactions
-- Just means we know transaction input, hence know TxOutAux corresponding to it,
-- Nothing otherwise.
type TxUndo = NonEmpty (Maybe TxOutAux)

type TxpUndo = [TxUndo]

----------------------------------------------------------------------------
-- TH instances
----------------------------------------------------------------------------

makePrisms ''TxOut
