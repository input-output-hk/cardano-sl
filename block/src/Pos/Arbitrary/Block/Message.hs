
module Pos.Arbitrary.Block.Message
       (
       ) where

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Block ()
import           Pos.Arbitrary.Txp ()
import           Pos.Arbitrary.Update ()
import qualified Pos.Block.Network.Types as T
import           Pos.Core (HasConfiguration)

------------------------------------------------------------------------------------------
-- Block network types
------------------------------------------------------------------------------------------

instance HasConfiguration => Arbitrary T.MsgGetHeaders where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasConfiguration => Arbitrary T.MsgGetBlocks where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance ( HasConfiguration ) =>
         Arbitrary T.MsgHeaders where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance ( HasConfiguration ) => Arbitrary T.MsgBlock where
    arbitrary = genericArbitrary
    shrink = genericShrink
