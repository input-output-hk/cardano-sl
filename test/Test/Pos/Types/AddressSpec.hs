-- | Specification of Pos.Types.Address.

module Test.Pos.Types.AddressSpec
       ( spec
       ) where

import           Data.Default          (def)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import           Pos.Binary            ()
import           Pos.Data.Attributes   (mkAttributes)
import           Pos.Types.Address     (AddrPkAttrs (..), Address (..), unsafeAddressHash)

spec :: Spec
spec = describe "Address" $ do
    prop "PK and SH addresses with same hashes are shown differently"
         pkAndShAreShownDifferently
    prop "PK and HDW addresses with same hashes are shown differently"
         pkAndHdwAreShownDifferently

pkAndShAreShownDifferently :: Word8 -> Bool
pkAndShAreShownDifferently x =
    (show (PubKeyAddress h def)) /=
    (show (ScriptAddress h) :: Text)
  where
    h = unsafeAddressHash x

pkAndHdwAreShownDifferently :: Word8 -> Bool
pkAndHdwAreShownDifferently x =
    (show (PubKeyAddress h (mkAttributes (AddrPkAttrs Nothing)))) /=
    (show (PubKeyAddress h (mkAttributes (AddrPkAttrs (Just [123])))) :: Text)
  where
    h = unsafeAddressHash x
