module Pos.Core.Txp.TxWitness
       ( TxWitness
       , TxInWitness (..)
       , TxSigData (..)
       , TxSig
       )where

import           Universum

import qualified Data.ByteString.Lazy as LBS
import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, (%))
import           Serokell.Util.Base16 (base16F)

import           Pos.Binary.Class (Bi (..), decodeKnownCborDataItem,
                     decodeListLenCanonical, decodeUnknownCborDataItem,
                     encodeKnownCborDataItem, encodeListLen,
                     encodeUnknownCborDataItem, matchSize)
import           Pos.Core.Common (Script, addressHash)
import           Pos.Crypto (Hash, PublicKey, RedeemPublicKey, RedeemSignature,
                     Signature, hash, shortHashF)

import           Pos.Core.Txp.Tx (Tx)

-- | A witness is a proof that a transaction is allowed to spend the funds it
-- spends (by providing signatures, redeeming scripts, etc). A separate proof
-- is provided for each input.
type TxWitness = Vector TxInWitness

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

instance Bi TxInWitness where
    encode input = case input of
        PkWitness key sig         ->
            encodeListLen 2 <>
            encode (0 :: Word8) <>
            encodeKnownCborDataItem (key, sig)
        ScriptWitness val red     ->
            encodeListLen 2 <>
            encode (1 :: Word8) <>
            encodeKnownCborDataItem (val, red)
        RedeemWitness key sig     ->
            encodeListLen 2 <>
            encode (2 :: Word8) <>
            encodeKnownCborDataItem (key, sig)
        UnknownWitnessType tag bs ->
            encodeListLen 2 <>
            encode tag <>
            encodeUnknownCborDataItem (LBS.fromStrict bs)
    decode = do
        len <- decodeListLenCanonical
        tag <- decode @Word8
        case tag of
            0 -> do
                matchSize len "TxInWitness.PkWitness" 2
                uncurry PkWitness <$> decodeKnownCborDataItem
            1 -> do
                matchSize len "TxInWitness.ScriptWitness" 2
                uncurry ScriptWitness <$> decodeKnownCborDataItem
            2 -> do
                matchSize len "TxInWitness.RedeemWitness" 2
                uncurry RedeemWitness <$> decodeKnownCborDataItem
            _ -> do
                matchSize len "TxInWitness.UnknownWitnessType" 2
                UnknownWitnessType tag <$> decodeUnknownCborDataItem

instance NFData TxInWitness

-- | Data that is being signed when creating a TxSig.
data TxSigData = TxSigData
    { -- | Transaction that we're signing
      txSigTxHash      :: !(Hash Tx)
    }
    deriving (Eq, Show, Generic, Typeable)

instance Bi TxSigData where
    encode (TxSigData {..}) = encode txSigTxHash
    decode = TxSigData <$> decode

-- | 'Signature' of addrId.
type TxSig = Signature TxSigData

deriveSafeCopySimple 0 'base ''TxInWitness
