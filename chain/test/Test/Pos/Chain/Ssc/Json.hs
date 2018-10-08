{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Pos.Chain.Ssc.Json
       ( tests
       ) where
import           Universum

import qualified Cardano.Crypto.Wallet as CC
import           Hedgehog (Property)
import qualified Hedgehog as H
import           Pos.Chain.Security (AttackTarget (..))
import           Pos.Crypto.Hashing (abstractHash)
import           Pos.Crypto.Signing (PublicKey (..))
import           Test.Pos.Chain.Ssc.Gen (genAttackTarget)
import           Test.Pos.Util.Golden (discoverGolden, eachOf,
                     goldenTestJSONPretty)
import           Test.Pos.Util.Tripping (discoverRoundTrip, roundTripsAesonShow)

-------------------------------------------------------------------------------
-- AttackTarget
-------------------------------------------------------------------------------
golden_AttackTarget_NetworkAddressTarget :: Property
golden_AttackTarget_NetworkAddressTarget =
    goldenTestJSONPretty exampleAttackTarget_NetworkAddressTarget
        "test/golden/AttackTarget_NetworkAddressTarget"

exampleAttackTarget_NetworkAddressTarget :: AttackTarget
exampleAttackTarget_NetworkAddressTarget = NetworkAddressTarget ("ggv", 32313)


golden_AttackTarget_PubKeyAddressTarget :: Property
golden_AttackTarget_PubKeyAddressTarget =
    goldenTestJSONPretty exampleAttackTarget_PubKeyAddressTarget
        "test/golden/AttackTarget_PubKeyAddressTarget"

exampleAttackTarget_PubKeyAddressTarget :: AttackTarget
exampleAttackTarget_PubKeyAddressTarget =
    PubKeyAddressTarget
        $ abstractHash (PublicKey (CC.XPub {CC.xpubPublicKey = pubKey1
          , CC.xpubChaincode = CC.ChainCode "Test"}))
  where
    pubKey1 = "\145\&3\131kUF\226\131\253M\174\157;w>\156k"

roundTripAttackTarget :: Property
roundTripAttackTarget =
    eachOf 1000 genAttackTarget roundTripsAesonShow

-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkParallel $$discoverRoundTrip
