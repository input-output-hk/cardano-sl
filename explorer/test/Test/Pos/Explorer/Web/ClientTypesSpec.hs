-- | This module is testing the ClientTypes module.

module Test.Pos.Explorer.Web.ClientTypesSpec
       ( spec
       ) where

import           Prelude                      (id)
import           Universum

import           Pos.Crypto
import           Pos.Explorer.Web.ClientTypes
import           Pos.Txp                      (TxId)
import           Test.Hspec                   (Spec, describe, it, shouldBe,
                                               shouldSatisfy)


-- stack test cardano-sl-explorer --fast --test-arguments "-m Test.Pos.Explorer.Web"
spec :: Spec
spec = describe "ClientTypes" $ do
    describe "CTxId serialization" $ do
        it "should encode Text into CTxId" $ do
            let cTxIdText = "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02"

  -- Serokell.Util.Base16.encode "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02"
  -- "62323966613137313536323735613835383938353733373662666165656566343766313834366638326561343932613830386535633631353562343530653032"

  -- Serokell.Util.Base16.decode "62323966613137313536323735613835383938353733373662666165656566343766313834366638326561343932613830386535633631353562343530653032"
  -- Right "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02"

            let decodedCTxId :: Either Text TxId
                decodedCTxId = decodeHash cTxIdText

            decodedCTxId `shouldSatisfy` isRight

            let result :: Text
                result = either id encodeHashHex decodedCTxId

            -- encodeHashHex . decodeHashHex $ "TEXT" == "TEXT"
            -- The gist is - (either id encodeHashHex $ decodeHashHex @TxId cTxIdText) `shouldBe` cTxIdText
            result `shouldBe` "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02"

            -- Uncomment next lines and prepare to be amazed! AFAIU it fails at `Bi.decodeFull`.
            -- decodeHashHex "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02" =
            -- Left "decodeFull failed for AbstractHash Blake2b_256 Tx: DeserialiseFailure 0 \"expected bytes\""

{-
            let decodedResult :: Either Text TxId
                decodedResult = decodeHashHex result

            decodedResult `shouldBe` decodedCTxId
-}
    describe "TxId serialization" $ do
        it "should encode Text into TxId" $ do
            let cTxIdText = "bd019a7759900ebc400830ec72fac3c2b1a6128fb71e94520cb60798360c1f13"

            let decodedCTxId :: Either Text TxId
                decodedCTxId = decodeHash cTxIdText

            decodedCTxId `shouldSatisfy` isRight
