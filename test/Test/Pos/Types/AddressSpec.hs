-- | Specification of Pos.Types.Address.

module Test.Pos.Types.AddressSpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import           Pos.Binary            ()
import           Pos.Types.Address     (Address (..), unsafeAddressHash)

spec :: Spec
spec = describe "Address" $ do
    prop "PK and SH addresses with same hashes are shown differently"
         pkAndShAreShownDifferently

pkAndShAreShownDifferently :: Word8 -> Bool
pkAndShAreShownDifferently x =
    (show (PubKeyAddress h)) /=
    (show (ScriptAddress h) :: Text)
  where
    h = unsafeAddressHash x
