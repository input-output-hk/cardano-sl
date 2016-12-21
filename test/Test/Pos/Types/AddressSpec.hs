-- | Specification of Pos.Types.Address.

module Test.Pos.Types.AddressSpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import           Pos.Binary            ()
import           Pos.Types.Address     (Address (..), AddressDestination (..),
                                        AddressVersion (..), unsafeAddressHash)

spec :: Spec
spec = describe "Address" $ do
    prop "PK and SH addresses with same hashes are shown differently"
         pkAndShAreShownDifferently

pkAndShAreShownDifferently :: Int -> Bool
pkAndShAreShownDifferently x =
    (show (Address (AddressVersion 0) (PubKeyDestination h) Nothing)) /=
    (show (Address (AddressVersion 0) (ScriptDestination h) Nothing) :: Text)
  where
    h = unsafeAddressHash x
