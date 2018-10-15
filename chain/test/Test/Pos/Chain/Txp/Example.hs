module Test.Pos.Chain.Txp.Example
       ( exampleTxId
       , exampleTxInList
       , exampleTxInUnknown
       , exampleTxInUtxo
       , exampleTxPayload
       , exampleTxProof
       , exampleTxOut
       , exampleTxOutList
       , exampleTxSig
       , exampleTxSigData
       , exampleTxpUndo
       , exampleTxWitness
       , exampleRedeemSignature
       , exampleHashTx
       ) where

import           Universum

import           Data.Coerce (coerce)
import           Data.List.NonEmpty (fromList)
import           Data.Maybe (fromJust)
import qualified Data.Vector as V

import qualified Cardano.Crypto.Wallet as CC
import           Pos.Chain.Txp (Tx (..), TxAux (..), TxId, TxIn (..),
                     TxInWitness (..), TxOut (..), TxOutAux (..),
                     TxPayload (..), TxProof (..), TxSig, TxSigData (..),
                     TxWitness, TxpUndo, mkTxPayload)
import           Pos.Core.Attributes (mkAttributes)
import           Pos.Core.Common (Coin (..), IsBootstrapEraAddr (..),
                     makePubKeyAddress)
import           Pos.Core.Merkle (mkMerkleTree, mtRoot)
import           Pos.Core.NetworkMagic (NetworkMagic (..))
import           Pos.Crypto (AbstractHash (..), Hash, PublicKey (..),
                     RedeemSignature, SignTag (..), hash,
                     redeemDeterministicKeyGen, redeemSign, sign)

import           Test.Pos.Core.ExampleHelpers (examplePublicKey,
                     exampleSecretKey)
import           Test.Pos.Crypto.Bi (exampleProtocolMagic, getBytes)

exampleTxAux :: TxAux
exampleTxAux = TxAux tx exampleTxWitness
  where
    tx = UnsafeTx exampleTxInList exampleTxOutList (mkAttributes ())

exampleTxId :: TxId
exampleTxId = exampleHashTx

exampleTxInList :: (NonEmpty TxIn)
exampleTxInList = fromList [exampleTxInUtxo]

exampleTxInUnknown :: TxIn
exampleTxInUnknown = TxInUnknown 47 ("forty seven" :: ByteString)

exampleTxInUtxo :: TxIn
exampleTxInUtxo = TxInUtxo exampleHashTx 47 -- TODO: loop here

exampleTxOut :: TxOut
exampleTxOut = TxOut (makePubKeyAddress NetworkMainOrStage (IsBootstrapEraAddr True) pkey) (Coin 47)
    where
        Right pkey = PublicKey <$> CC.xpub (getBytes 0 64)

exampleTxOutList :: (NonEmpty TxOut)
exampleTxOutList = fromList [exampleTxOut]

exampleTxPayload :: TxPayload
exampleTxPayload = mkTxPayload [exampleTxAux]

exampleTxProof :: TxProof
exampleTxProof = TxProof 32 mroot hashWit
  where
    mroot = mtRoot $ mkMerkleTree [(UnsafeTx exampleTxInList exampleTxOutList (mkAttributes ()))]
    hashWit = hash $ [(V.fromList [(PkWitness examplePublicKey exampleTxSig)])]

exampleTxSig :: TxSig
exampleTxSig = sign exampleProtocolMagic SignForTestingOnly exampleSecretKey exampleTxSigData

exampleTxSigData :: TxSigData
exampleTxSigData = TxSigData exampleHashTx

exampleTxpUndo :: TxpUndo
exampleTxpUndo = [Just . TxOutAux <$> exampleTxOutList]

exampleTxWitness :: TxWitness
exampleTxWitness = V.fromList [(PkWitness examplePublicKey exampleTxSig)]

exampleRedeemSignature :: RedeemSignature TxSigData
exampleRedeemSignature = redeemSign exampleProtocolMagic SignForTestingOnly rsk exampleTxSigData
    where
        rsk = fromJust (snd <$> redeemDeterministicKeyGen (getBytes 0 32))

exampleHashTx :: Hash Tx
exampleHashTx = coerce (hash "golden" :: Hash Text)
