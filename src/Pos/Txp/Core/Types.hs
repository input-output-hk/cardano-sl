{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Core types of Txp component, i. e. types which actually form
-- block or are used by other components.

module Pos.Txp.Core.Types
       ( TxId

       -- * Witness
       , TxInWitness (..)
       , TxWitness
       , TxDistribution (..)
       , TxSigData
       , TxSig

       -- * Tx parts
       , TxIn (..)
       , toPair
       , TxOut (..)
       , TxOutAux
       , txOutStake
       , TxAttributes

       -- * Tx
       , Tx (..)
       , TxAux
       , mkTx
       , txInputs
       , txOutputs
       , txAttributes
       , txF
       , txaF

       -- * Undo
       , TxUndo
       , TxpUndo
       ) where

import           Control.Lens         (makeLenses)
import           Data.DeriveTH        (derive, makeNFData)
import           Data.Hashable        (Hashable)
import qualified Data.Text            as T
import           Data.Text.Buildable  (Buildable)
import qualified Data.Text.Buildable  as Buildable
import           Data.Vector          (Vector)
import           Formatting           (Format, bprint, build, int, later, sformat, (%))
import           Serokell.Util.Base16 (base16F)
import           Serokell.Util.Text   (listBuilderJSON, listJson, listJsonIndent,
                                       pairBuilder)
import           Serokell.Util.Verify (VerificationRes (..), verifyGeneric)
import           Universum

import           Pos.Binary.Core      ()
import           Pos.Binary.Crypto    ()
import           Pos.Core.Address     ()
import           Pos.Core.Types       (Address (..), Coin, Script, StakeholderId, coinF,
                                       mkCoin)
import           Pos.Crypto           (Hash, PublicKey, Signature, hash, shortHashF)
import           Pos.Data.Attributes  (Attributes)

-- | Represents transaction identifier as 'Hash' of 'Tx'.
type TxId = Hash Tx

----------------------------------------------------------------------------
-- Witness
----------------------------------------------------------------------------

-- | Data that is being signed when creating a TxSig.
type TxSigData = (TxId, Word32, Hash [TxOut], Hash TxDistribution)

-- | 'Signature' of addrId.
type TxSig = Signature TxSigData

-- | A witness for a single input.
data TxInWitness
    = PkWitness { twKey :: !PublicKey
                , twSig :: !TxSig }
    | ScriptWitness { twValidator :: !Script
                    , twRedeemer  :: !Script }
    | UnknownWitnessType !Word8 !ByteString
    deriving (Eq, Show, Generic, Typeable)

instance Hashable TxInWitness

instance Buildable TxInWitness where
    build (PkWitness key sig) =
        bprint ("PkWitness: key = "%build%", sig = "%build) key sig
    build (ScriptWitness val red) =
        bprint ("ScriptWitness: "%
                "validator hash = "%shortHashF%", "%
                "redeemer hash = "%shortHashF) (hash val) (hash red)
    build (UnknownWitnessType t bs) =
        bprint ("UnknownWitnessType "%build%" "%base16F) t bs

-- | A witness is a proof that a transaction is allowed to spend the funds it
-- spends (by providing signatures, redeeming scripts, etc). A separate proof
-- is provided for each input.
type TxWitness = Vector TxInWitness

-- | Distribution of “fake” stake that follow-the-satoshi would use for a
-- particular transaction.
newtype TxDistribution = TxDistribution {
    getTxDistribution :: [[(StakeholderId, Coin)]] }
    deriving (Eq, Show, Generic, Typeable)

instance Buildable TxDistribution where
    build (TxDistribution x) =
        listBuilderJSON . map (listBuilderJSON . map pairBuilder) $ x

----------------------------------------------------------------------------
-- Tx parts
----------------------------------------------------------------------------

-- | Transaction input.
data TxIn = TxIn
    { -- | Which transaction's output is used
      txInHash  :: !TxId
      -- | Index of the output in transaction's outputs
    , txInIndex :: !Word32
    } deriving (Eq, Ord, Show, Generic, Typeable)

instance Hashable TxIn

instance Buildable TxIn where
    build TxIn {..} = bprint ("TxIn "%shortHashF%" #"%int) txInHash txInIndex

-- | Make pair from TxIn
toPair :: TxIn -> (TxId, Word32)
toPair (TxIn h i) = (h, i)

-- | Transaction output.
data TxOut = TxOut
    { txOutAddress :: !Address
    , txOutValue   :: !Coin
    } deriving (Eq, Ord, Generic, Show, Typeable)

instance Hashable TxOut

instance Buildable TxOut where
    build TxOut {..} =
        bprint ("TxOut "%coinF%" -> "%build) txOutValue txOutAddress

type TxOutAux = (TxOut, [(StakeholderId, Coin)])

instance Buildable TxOutAux where
    build (out, distr) =
        bprint ("{txout = "%build%", distr = "%listJson%"}")
               out (map pairBuilder distr)

-- | Use this function if you need to know how a 'TxOut' distributes stake
-- (e.g. for the purpose of running follow-the-satoshi).
txOutStake :: TxOutAux -> [(StakeholderId, Coin)]
txOutStake (TxOut{..}, mb) = case txOutAddress of
    PubKeyAddress x _ -> [(x, txOutValue)]
    _                 -> mb

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
    { _txInputs     :: ![TxIn]   -- ^ Inputs of transaction.
    , _txOutputs    :: ![TxOut]  -- ^ Outputs of transaction.
    , _txAttributes :: !TxAttributes -- ^ Attributes of transaction
    } deriving (Eq, Ord, Generic, Show, Typeable)

makeLenses ''Tx

-- | Transaction + auxiliary data
type TxAux = (Tx, TxWitness, TxDistribution)

instance Hashable Tx

instance Buildable Tx where
    build UnsafeTx {..} =
        bprint
            ("Transaction with inputs "%listJson%", outputs: "%listJson)
            _txInputs _txOutputs

-- | Specialized formatter for 'Tx'.
txF :: Format r (Tx -> r)
txF = build

-- | Specialized formatter for 'Tx' with auxiliary data
txaF :: Format r (TxAux -> r)
txaF = later $ \(tx, w, d) ->
    bprint (build%"\n"%
            "witnesses: "%listJsonIndent 4%"\n"%
            "distribution: "%build) tx w d

-- | Create valid Tx or fail.
-- Verify inputs and outputs are non empty; have enough coins.
mkTx :: MonadFail m => [TxIn] -> [TxOut] -> TxAttributes -> m Tx
mkTx inputs outputs attrs
    | null inputs = fail "transaction doesn't have inputs"
    | null outputs = fail "transaction doesn't have outputs"
    | VerFailure ers <- verifyOutputs =
        fail $ T.unpack $ T.intercalate "; " ers
    | otherwise = pure $ UnsafeTx inputs outputs attrs
  where
    verifyOutputs = verifyGeneric $ concat $
                    zipWith outputPredicates [0..] outputs
    outputPredicates (i :: Word) TxOut{..} = [
      ( txOutValue > mkCoin 0
      , sformat ("output #"%int%" has non-positive value: "%coinF)
                i txOutValue) ]

----------------------------------------------------------------------------
-- Undo
----------------------------------------------------------------------------

-- | Particular undo needed for transactions
type TxUndo = [TxOutAux]

type TxpUndo = [TxUndo]

----------------------------------------------------------------------------
-- TH instances
----------------------------------------------------------------------------

derive makeNFData ''TxIn
derive makeNFData ''TxInWitness
derive makeNFData ''TxOut
derive makeNFData ''TxDistribution
derive makeNFData ''Tx
