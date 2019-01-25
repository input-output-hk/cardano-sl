{-# LANGUAGE TypeApplications #-}

module Test.Pos.Chain.Txp.Gen
       ( genTxpConfiguration
       , genPkWitness
       , genRedeemWitness
       , genScriptWitness
       , genTx
       , genTxAttributes
       , genTxAux
       , genTxHash
       , genTxId
       , genTxIn
       , genTxInList
       , genTxInWitness
       , genTxOut
       , genTxOutAux
       , genTxOutList
       , genTxpUndo
       , genTxPayload
       , genTxProof
       , genTxSig
       , genTxSigData
       , genTxValidationRulesConfig
       , genTxUndo
       , genTxWitness
       , genUnknownWitnessType
       ) where

import           Universum

import           Data.ByteString.Base16 as B16
import           Data.Coerce (coerce)
import qualified Data.Set as S
import qualified Data.Vector as V
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Chain.Txp (Tx (..), TxAttributes, TxAux (..), TxId,
                     TxIn (..), TxInWitness (..), TxOut (..), TxOutAux (..),
                     TxPayload, TxProof (..), TxSig, TxSigData (..), TxUndo,
                     TxValidationRulesConfig (..), TxWitness,
                     TxpConfiguration (..), TxpUndo, mkTxPayload)
import           Pos.Core.Attributes (mkAttributes)
import           Pos.Crypto (Hash, ProtocolMagic, decodeHash, sign)

import           Test.Pos.Core.Gen (gen32Bytes, genAddress, genBytes, genCoin,
                     genEpochIndex, genMerkleRoot, genScript, genTextHash,
                     genWord32)
import           Test.Pos.Crypto.Gen (genAbstractHash, genPublicKey,
                     genRedeemPublicKey, genRedeemSignature, genSecretKey,
                     genSignTag)

genTxpConfiguration :: Gen TxpConfiguration
genTxpConfiguration = do
    limit <- Gen.int (Range.constant 0 200)
    addrs <- Gen.list (Range.linear 0 50) genAddress
    return (TxpConfiguration limit (S.fromList addrs))

genPkWitness :: ProtocolMagic -> Gen TxInWitness
genPkWitness pm = PkWitness <$> genPublicKey <*> genTxSig pm

genRedeemWitness :: ProtocolMagic -> Gen TxInWitness
genRedeemWitness pm =
    RedeemWitness <$> genRedeemPublicKey <*> genRedeemSignature pm genTxSigData

genTxValidationRulesConfig :: Gen TxValidationRulesConfig
genTxValidationRulesConfig =
    TxValidationRulesConfig <$> genEpochIndex
                            <*> Gen.integral (Range.constant 1 1000)
                            <*> Gen.integral (Range.constant 1 1000)

genScriptWitness :: Gen TxInWitness
genScriptWitness = ScriptWitness <$> genScript <*> genScript

genTx :: Gen Tx
genTx = UnsafeTx <$> genTxInList <*> genTxOutList <*> genTxAttributes

genTxAttributes :: Gen TxAttributes
genTxAttributes = pure $ mkAttributes ()

genTxAux :: ProtocolMagic -> Gen TxAux
genTxAux pm = TxAux <$> genTx <*> (genTxWitness pm)

genTxHash :: Gen (Hash Tx)
genTxHash = coerce <$> genTextHash

genTxId :: Gen TxId
genTxId = genBase16Text >>= pure . decodeHash >>= either error pure
    where
        genBase16Text = decodeUtf8 @Text @ByteString <$> genBase16Bs

genBase16Bs :: Gen ByteString
genBase16Bs = B16.encode <$> genBytes 32

--genTxId :: Gen TxId
--genTxId = coerce <$> genTxHash

genTxIn :: Gen TxIn
genTxIn = Gen.choice gens
  where
    gens = [ TxInUtxo <$> genTxId <*> genWord32
           -- 0 is reserved for TxInUtxo tag ----------+
           , TxInUnknown <$> Gen.word8 (Range.constant 1 255)
                         <*> gen32Bytes
           ]

genTxInList :: Gen (NonEmpty TxIn)
genTxInList = Gen.nonEmpty (Range.linear 1 20) genTxIn

genTxOut :: Gen TxOut
genTxOut = TxOut <$> genAddress <*> genCoin

genTxOutAux :: Gen TxOutAux
genTxOutAux = TxOutAux <$> genTxOut

genTxOutList :: Gen (NonEmpty TxOut)
genTxOutList = Gen.nonEmpty (Range.linear 1 100) genTxOut

genTxpUndo :: Gen TxpUndo
genTxpUndo = Gen.list (Range.linear 1 50) genTxUndo

genTxPayload :: ProtocolMagic -> Gen TxPayload
genTxPayload pm = mkTxPayload <$> (Gen.list (Range.linear 0 10) (genTxAux pm))

genTxProof :: ProtocolMagic -> Gen TxProof
genTxProof pm =
    TxProof
        <$> genWord32
        <*> genMerkleRoot genTx
        <*> genAbstractHash (Gen.list (Range.linear 1 5) (genTxWitness pm))

genTxSig :: ProtocolMagic -> Gen TxSig
genTxSig pm =
    sign pm <$> genSignTag <*> genSecretKey <*> genTxSigData

genTxSigData :: Gen TxSigData
genTxSigData = TxSigData <$> genTxHash

genTxInWitness :: ProtocolMagic -> Gen TxInWitness
genTxInWitness pm = Gen.choice gens
  where
    gens = [ genPkWitness pm
           , genRedeemWitness pm
           , genScriptWitness
           , genUnknownWitnessType
           ]

genTxUndo :: Gen TxUndo
genTxUndo = Gen.nonEmpty (Range.linear 1 10) $ Gen.maybe genTxOutAux

genTxWitness :: ProtocolMagic -> Gen TxWitness
genTxWitness pm = V.fromList <$> Gen.list (Range.linear 1 10) (genTxInWitness pm)

genUnknownWitnessType :: Gen TxInWitness
genUnknownWitnessType =
    UnknownWitnessType <$> Gen.word8 (Range.constant 3 maxBound) <*> gen32Bytes
