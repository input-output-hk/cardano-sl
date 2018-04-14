module Pos.Arbitrary.Data.Attributes where

import           Universum

import qualified Data.ByteString.Lazy as BSL
import           Test.QuickCheck (Arbitrary (..))
import           Data.ByteString.Arbitrary (ArbByteString (..))

import           Pos.Data.Attributes


instance Arbitrary UnparsedFields where
    arbitrary = UnparsedFields . (fmap $ BSL.fromStrict . fromABS) <$> arbitrary
    shrink (UnparsedFields m) = map (UnparsedFields . fmap (BSL.fromStrict . fromABS)) $ shrink (ABS . BSL.toStrict <$> m)

instance (Arbitrary h) => Arbitrary (Attributes h) where
    arbitrary = Attributes <$> arbitrary <*> arbitrary
    shrink Attributes {..} = [ Attributes d r | d <- shrink attrData
                                              , r <- shrink attrRemain ]
