{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Pos.Chain.Ssc.Json
       ( tests
       ) where
import           Universum

import qualified Cardano.Crypto.Wallet as CC
import           Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LB
import           Hedgehog (Property)
import qualified Hedgehog as H
import           Hedgehog.Internal.Property (assert, failWith, withTests)

import           Pos.Chain.Security (AttackTarget (..))
import           Pos.Crypto.Hashing (abstractHash)
import           Pos.Crypto.Signing (PublicKey (..))

import           Test.Pos.Chain.Ssc.Gen (genAttackTarget)
import           Test.Pos.Util.Golden (discoverGolden, eachOf,
                     goldenTestJSONPretty, goldenValueEquiv)
import           Test.Pos.Util.Tripping (discoverRoundTrip, roundTripsAesonShow)

-------------------------------------------------------------------------------
-- AttackTarget
-------------------------------------------------------------------------------
golden_AttackTarget_NetworkAddressTarget :: Property
golden_AttackTarget_NetworkAddressTarget =
    goldenTestJSONPretty exampleAttackTarget_NetworkAddressTarget
        "test/golden/json/AttackTarget_NetworkAddressTarget"

exampleAttackTarget_NetworkAddressTarget :: AttackTarget
exampleAttackTarget_NetworkAddressTarget = NetworkAddressTarget ("ggv", 32313)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

golden_prettyEquivalence_AttackTarget_NetworkAddressTarget :: Property
golden_prettyEquivalence_AttackTarget_NetworkAddressTarget =
    withFrozenCallStack $ do
        withTests 1 . H.property $ do
            prettyJ <- liftIO $ LB.readFile pFile
            oldJ <- liftIO $ LB.readFile oFile
            let equivTest = goldenValueEquiv
                                (eitherDecode prettyJ :: Either String AttackTarget)
                                (eitherDecode oldJ :: Either String AttackTarget)
            case equivTest of
                Left err    -> failWith Nothing $ "could not decode: " <> show err
                Right bool' -> assert bool'
  where
    pFile = "test/golden/json/AttackTarget_NetworkAddressTarget"
    oFile = "test/golden/oldJson/AttackTarget_NetworkAddressTarget"

golden_AttackTarget_PubKeyAddressTarget :: Property
golden_AttackTarget_PubKeyAddressTarget =
    goldenTestJSONPretty exampleAttackTarget_PubKeyAddressTarget
        "test/golden/json/AttackTarget_PubKeyAddressTarget"

exampleAttackTarget_PubKeyAddressTarget :: AttackTarget
exampleAttackTarget_PubKeyAddressTarget =
    PubKeyAddressTarget
        $ abstractHash (PublicKey (CC.XPub {CC.xpubPublicKey = pubKey1
          , CC.xpubChaincode = CC.ChainCode "Test"}))
  where
    pubKey1 = "\145\&3\131kUF\226\131\253M\174\157;w>\156k"

golden_prettyEquivalence_AttackTarget_PubKeyAddressTarget :: Property
golden_prettyEquivalence_AttackTarget_PubKeyAddressTarget =
    withFrozenCallStack $ do
        withTests 1 . H.property $ do
            prettyJ <- liftIO $ LB.readFile pFile
            oldJ <- liftIO $ LB.readFile oFile
            let equivTest = goldenValueEquiv
                                (eitherDecode prettyJ :: Either String AttackTarget)
                                (eitherDecode oldJ :: Either String AttackTarget)
            case equivTest of
                Left err    -> failWith Nothing $ "could not decode: " <> show err
                Right bool' -> assert bool'
  where
    pFile = "test/golden/json/AttackTarget_PubKeyAddressTarget"
    oFile = "test/golden/oldJson/AttackTarget_PubKeyAddressTarget"

roundTripAttackTarget :: Property
roundTripAttackTarget =
    eachOf 1000 genAttackTarget roundTripsAesonShow

-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkParallel $$discoverRoundTrip
