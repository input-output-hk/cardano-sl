-- | Specification of Pos.Core.Address.

module Test.Pos.Core.AddressSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)

import           Pos.Arbitrary.Core    ()
import           Pos.Core              (makePubKeyAddress, makePubKeyHdwAddress)
import           Pos.Crypto            (PublicKey)
import           Pos.Crypto.HD         (HDAddressPayload (..))

spec :: Spec
spec = describe "Address" $ modifyMaxSuccess (min 10) $ do
    prop "PK and HDW addresses with same public key are shown differently"
         pkAndHdwAreShownDifferently

pkAndHdwAreShownDifferently :: PublicKey -> Bool
pkAndHdwAreShownDifferently pk =
    (show (makePubKeyAddress pk)) /=
    (show @_ @Text (makePubKeyHdwAddress (HDAddressPayload "pataq") pk))
