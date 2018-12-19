{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.Pos.Chain.Txp.Bi
       ( tests
       ) where

import           Universum

import qualified Data.Map as M
import           Data.Typeable (typeRep)
import           Hedgehog (Gen, Property)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen

import           Pos.Binary.Class (Bi, Case (..), LengthOf, SizeOverride (..),
                     szCases)
import           Pos.Chain.Txp (Tx (..), TxAux (..), TxIn (..),
                     TxInWitness (..), TxOut (..), TxOutAux (..),
                     TxSigData (..))
import           Pos.Core.Attributes (Attributes (..), mkAttributes)
import           Pos.Core.Common (AddrAttributes (..), Script (..))
import           Pos.Crypto (ProtocolMagic (..), ProtocolMagicId (..),
                     RequiresNetworkMagic (..), SignTag (..), Signature, sign)

import           Test.Pos.Binary.Helpers (SizeTestConfig (..), scfg, sizeTest)
import           Test.Pos.Binary.Helpers.GoldenRoundTrip (goldenTestBi,
                     roundTripsBiBuildable, roundTripsBiShow)
import           Test.Pos.Chain.Txp.Example (exampleHashTx,
                     exampleRedeemSignature, exampleTxId, exampleTxInList,
                     exampleTxInUnknown, exampleTxInUtxo, exampleTxOut,
                     exampleTxOutList, exampleTxProof, exampleTxSig,
                     exampleTxSigData, exampleTxWitness)
import           Test.Pos.Chain.Txp.Gen (genTx, genTxAttributes, genTxAux,
                     genTxHash, genTxId, genTxIn, genTxInList, genTxInWitness,
                     genTxOut, genTxOutAux, genTxOutList, genTxPayload,
                     genTxProof, genTxSig, genTxSigData, genTxWitness)
import           Test.Pos.Core.ExampleHelpers (examplePublicKey,
                     exampleRedeemPublicKey, exampleSecretKey, feedPM)
import           Test.Pos.Util.Golden (discoverGolden, eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip)

--------------------------------------------------------------------------------
-- Tx
--------------------------------------------------------------------------------

golden_Tx :: Property
golden_Tx = goldenTestBi tx "test/golden/bi/txp/Tx"
    where
        tx = UnsafeTx exampleTxInList exampleTxOutList (mkAttributes ())

roundTripTx :: Property
roundTripTx = eachOf 50 genTx roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxAttributes
--------------------------------------------------------------------------------

golden_TxAttributes :: Property
golden_TxAttributes = goldenTestBi txA "test/golden/bi/txp/TxAttributes"
    where
        txA = mkAttributes ()


roundTripTxAttributes :: Property
roundTripTxAttributes = eachOf 10 genTxAttributes roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxAux
--------------------------------------------------------------------------------

roundTripTxAux :: Property
roundTripTxAux = eachOf 100 (feedPM genTxAux) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Tx Hash
--------------------------------------------------------------------------------

golden_HashTx :: Property
golden_HashTx = goldenTestBi exampleHashTx "test/golden/bi/txp/HashTx"

roundTripHashTx :: Property
roundTripHashTx = eachOf 50 genTxHash roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxIn
--------------------------------------------------------------------------------


golden_TxInUtxo :: Property
golden_TxInUtxo = goldenTestBi exampleTxInUtxo "test/golden/bi/txp/TxIn_Utxo"

golden_TxInUnknown :: Property
golden_TxInUnknown = goldenTestBi exampleTxInUnknown "test/golden/bi/txp/TxIn_Unknown"

roundTripTxIn :: Property
roundTripTxIn = eachOf 100 genTxIn roundTripsBiBuildable


--------------------------------------------------------------------------------
-- TxId
--------------------------------------------------------------------------------

golden_TxId :: Property
golden_TxId = goldenTestBi exampleTxId "test/golden/bi/txp/TxId"

roundTripTxId :: Property
roundTripTxId = eachOf 50 genTxId roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxInList
--------------------------------------------------------------------------------

golden_TxInList :: Property
golden_TxInList = goldenTestBi exampleTxInList "test/golden/bi/txp/TxInList"

roundTripTxInList :: Property
roundTripTxInList = eachOf 50 genTxInList roundTripsBiShow

--------------------------------------------------------------------------------
-- TxInWitness
--------------------------------------------------------------------------------

golden_PkWitness :: Property
golden_PkWitness = goldenTestBi pkWitness "test/golden/bi/txp/TxInWitness_PkWitness"
     where
        pkWitness = PkWitness examplePublicKey exampleTxSig

golden_ScriptWitness :: Property
golden_ScriptWitness = goldenTestBi scriptWitness "test/golden/bi/txp/TxInWitness_ScriptWitness"
    where
        scriptWitness = ScriptWitness validatorScript redeemerScript
        validatorScript = Script 47 "serialized script"
        redeemerScript = Script 47 "serialized script"

golden_RedeemWitness :: Property
golden_RedeemWitness = goldenTestBi redeemWitness "test/golden/bi/txp/TxInWitness_RedeemWitness"
    where
        redeemWitness = RedeemWitness exampleRedeemPublicKey exampleRedeemSignature

golden_UnknownWitnessType :: Property
golden_UnknownWitnessType = goldenTestBi unkWitType "test/golden/bi/txp/TxInWitness_UnknownWitnessType"
    where
        unkWitType = UnknownWitnessType 47 "forty seven"

roundTripTxInWitness :: Property
roundTripTxInWitness = eachOf 50 (feedPM genTxInWitness) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxOutList
--------------------------------------------------------------------------------

golden_TxOutList :: Property
golden_TxOutList = goldenTestBi exampleTxOutList "test/golden/bi/txp/TxOutList"

roundTripTxOutList :: Property
roundTripTxOutList = eachOf 50 genTxOutList roundTripsBiShow

--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

golden_TxOut :: Property
golden_TxOut = goldenTestBi exampleTxOut "test/golden/bi/txp/TxOut"

roundTripTxOut :: Property
roundTripTxOut = eachOf 50 genTxOut roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxOutAux
--------------------------------------------------------------------------------

golden_TxOutAux :: Property
golden_TxOutAux =  goldenTestBi txOutAux "test/golden/bi/txp/TxOutAux"
    where
        txOutAux = TxOutAux exampleTxOut

roundTripTxOutAux :: Property
roundTripTxOutAux = eachOf 50 genTxOutAux roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxPayload
--------------------------------------------------------------------------------

roundTripTxPayload :: Property
roundTripTxPayload = eachOf 50 (feedPM genTxPayload) roundTripsBiShow

--------------------------------------------------------------------------------
-- TxProof
--------------------------------------------------------------------------------

golden_TxProof :: Property
golden_TxProof =  goldenTestBi exampleTxProof "test/golden/bi/txp/TxProof"

roundTripTxProof :: Property
roundTripTxProof = eachOf 50 (feedPM genTxProof) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxSig
--------------------------------------------------------------------------------

golden_TxSig :: Property
golden_TxSig = goldenTestBi txSigGold "test/golden/bi/txp/TxSig"
    where
        txSigGold = sign pm SignForTestingOnly
                         exampleSecretKey exampleTxSigData
        pm = ProtocolMagic { getProtocolMagicId = ProtocolMagicId 0
                           , getRequiresNetworkMagic = RequiresNoMagic
                           }

roundTripTxSig :: Property
roundTripTxSig = eachOf 50 (feedPM genTxSig) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxSigData
--------------------------------------------------------------------------------

golden_TxSigData :: Property
golden_TxSigData = goldenTestBi exampleTxSigData "test/golden/bi/txp/TxSigData"

roundTripTxSigData :: Property
roundTripTxSigData = eachOf 50 genTxSigData roundTripsBiShow

--------------------------------------------------------------------------------
-- TxWitness
--------------------------------------------------------------------------------

golden_TxWitness :: Property
golden_TxWitness = goldenTestBi exampleTxWitness "test/golden/bi/txp/TxWitness"

roundTripTxWitness :: Property
roundTripTxWitness = eachOf 20 (feedPM genTxWitness) roundTripsBiShow

sizeEstimates :: H.Group
sizeEstimates =
  let check :: (Show a, Bi a) => Gen a -> Property
      check g = sizeTest $ scfg { gen = g }
      pm = ProtocolMagic { getProtocolMagicId = ProtocolMagicId 0
                         , getRequiresNetworkMagic = RequiresNoMagic
                         }
      knownTxIn (TxInUnknown _ _) = False
      knownTxIn _                 = True

      -- Explicit bounds for types, based on the generators from Gen.
      attrUnitSize = (typeRep (Proxy @(Attributes ()))
                     , SizeConstant 1)
      attrAddrSize = (typeRep (Proxy @(Attributes AddrAttributes)),
                      SizeConstant (szCases [ Case "min" 1, Case "max" 1024 ]))
      txSigSize    = (typeRep (Proxy @(Signature TxSigData))
                     , SizeConstant 66)
      scriptSize   = (typeRep (Proxy @Script),
                      SizeConstant $ szCases [ Case "loScript" 1
                                             , Case "hiScript" 255 ])

  in H.Group "Encoded size bounds for core types."
        [ ("TxId"                 , check genTxId)
        , ("Tx"                   , sizeTest $ scfg
              { gen = genTx
              , addlCtx = M.fromList [ attrUnitSize, attrAddrSize ]
              , computedCtx = \tx -> M.fromList
                  [ (typeRep (Proxy @(LengthOf [TxIn])),
                     SizeConstant (fromIntegral $ length $ _txInputs tx))
                  , (typeRep (Proxy @(LengthOf [TxOut])),
                     SizeConstant (fromIntegral $ length $ _txOutputs tx))
                  ]
              })
        , ("TxIn"                 , check (Gen.filter knownTxIn genTxIn))
        , ("TxOut"                , sizeTest $ scfg
              { gen = genTxOut
              , addlCtx = M.fromList [ attrAddrSize ]
              })
        , ("TxAux"                , sizeTest $ scfg
              { gen = genTxAux pm
              , addlCtx = M.fromList [ attrUnitSize
                                     , attrAddrSize
                                     , scriptSize
                                     , txSigSize ]
              , computedCtx = \(TxAux tx witness) -> M.fromList
                  [ (typeRep (Proxy @(LengthOf [TxIn])),
                     SizeConstant (fromIntegral $ length $ _txInputs tx))
                  , (typeRep (Proxy @(LengthOf (Vector TxInWitness))),
                     SizeConstant (fromIntegral $ length witness))
                  , (typeRep (Proxy @(LengthOf [TxOut])),
                     SizeConstant (fromIntegral $ length $ _txOutputs tx))
                  ]
              })
        , ("TxInWitness"          , sizeTest $ scfg
              { gen = genTxInWitness pm
              , addlCtx = M.fromList [ txSigSize, scriptSize ]
              })
        , ("TxSigData"            , check genTxSigData)
        , ("Signature TxSigData"  , sizeTest $ scfg
              { gen = genTxSig pm
              , addlCtx = M.fromList [ txSigSize ]
              })
        ]

-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests = and <$> sequence
    [ H.checkSequential $$discoverGolden
    , H.checkParallel $$discoverRoundTrip
    , H.checkParallel sizeEstimates
    ]
