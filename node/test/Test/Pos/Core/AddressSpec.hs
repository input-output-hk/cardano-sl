-- | Specification of Pos.Core.Address.

module Test.Pos.Core.AddressSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)

import           Pos.Arbitrary.Core    ()
import           Pos.Core              (IsBootstrapEraAddr (..), makePubKeyAddress,
                                        makePubKeyHdwAddress)
import           Pos.Crypto            (PublicKey)
import           Pos.Crypto.HD         (HDAddressPayload (..))

spec :: Spec
spec = describe "Address" $ modifyMaxSuccess (min 10) $ do
    prop "PK and HDW addresses with same public key are shown differently"
         pkAndHdwAreShownDifferently

pkAndHdwAreShownDifferently :: Bool -> PublicKey -> Bool
pkAndHdwAreShownDifferently isBootstrap pk =
    show (makePubKeyAddress (IsBootstrapEraAddr isBootstrap) pk) /=
    (show @_ @Text (makePubKeyHdwAddress (IsBootstrapEraAddr isBootstrap)
                    (HDAddressPayload "pataq") pk))
