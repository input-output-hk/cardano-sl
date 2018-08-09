{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances for client types

module Pos.Arbitrary.Wallet.Web.ClientTypes
       (
       ) where

import           Universum

import qualified Data.ByteString.Char8 as B8
import           Pos.Wallet.Web.ClientTypes.Types (CHash (..), CId (..), CWAddressMeta (..))
import           Pos.Wallet.Web.State (WAddressMeta (..))
import qualified Serokell.Util.Base64 as B64
import           Test.QuickCheck (Arbitrary (..), vectorOf)

instance Arbitrary CHash where
    arbitrary = CHash . B64.encode . B8.pack <$> vectorOf 64 arbitrary

instance Arbitrary (CId w) where
    arbitrary = CId <$> arbitrary

-- TODO it's generate invalid CWAddressMeta
-- @deriveLvl2KeyPair@ should be used for this instance
-- but it's extremely slow
instance Arbitrary CWAddressMeta where
    arbitrary = do
        cwamWId <- arbitrary
        cwamAccountIndex <- arbitrary
        cwamAddressIndex <- arbitrary
        cwamId <- arbitrary
        pure CWAddressMeta {..}

instance Arbitrary WAddressMeta where
  arbitrary = WAddressMeta
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
