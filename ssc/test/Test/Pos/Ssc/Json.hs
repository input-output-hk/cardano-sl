{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Pos.Ssc.Json
       ( tests
       ) where
import           Universum

import qualified Cardano.Crypto.Wallet as CC
import           Hedgehog (Property)
import qualified Hedgehog as H
import           Pos.Crypto.Hashing (abstractHash)
import           Pos.Crypto.Signing (PublicKey (..))
import           Pos.Security.Params (AttackTarget (..))
import           Test.Pos.Ssc.Gen (genAttackTarget)
import           Test.Pos.Util.Golden (discoverGolden, eachOf, goldenTestJSON)
import           Test.Pos.Util.Tripping (discoverRoundTrip, roundTripsAesonShow)

-------------------------------------------------------------------------------
-- AttackTarget
-------------------------------------------------------------------------------
golden_AttackTarget_NetworkAddressTarget :: Property
golden_AttackTarget_NetworkAddressTarget =
    goldenTestJSON exampleAttackTarget_NetworkAddressTarget
        "test/golden/AttackTarget_NetworkAddressTarget"
  where
    exampleAttackTarget_NetworkAddressTarget =
        NetworkAddressTarget ("ggv", 32313)


golden_AttackTarget_PubKeyAddressTarget :: Property
golden_AttackTarget_PubKeyAddressTarget =
    goldenTestJSON exampleAttackTarget_PubKeyAddressTarget
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
