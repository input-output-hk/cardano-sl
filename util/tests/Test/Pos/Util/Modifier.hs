module Test.Pos.Util.Modifier where

import           Test.QuickCheck (Arbitrary)
import           Test.QuickCheck.Instances ()

deriving instance (Eq k, Hashable k, Arbitrary k, Arbitrary v) =>
    Arbitrary (MapModifier k v)

