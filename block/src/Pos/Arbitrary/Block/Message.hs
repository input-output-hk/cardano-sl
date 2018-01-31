
module Pos.Arbitrary.Block.Message
       (
       ) where

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Ssc (SscPayloadDependsOnSlot (..))
import           Pos.Arbitrary.Txp ()
import           Pos.Arbitrary.Update ()
import           Pos.Arbitrary.Block ()
import           Pos.Binary.Class (Bi, Raw)
import qualified Pos.Block.Network.Types as T
import           Pos.Core (HasConfiguration)
import           Pos.Core.Ssc (SscPayload, SscProof)

------------------------------------------------------------------------------------------
-- Block network types
------------------------------------------------------------------------------------------

instance Arbitrary T.MsgGetHeaders where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.MsgGetBlocks where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance ( Arbitrary SscPayload
         , Arbitrary SscProof
         , Bi Raw
         , HasConfiguration
         ) =>
         Arbitrary T.MsgHeaders where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance ( Arbitrary SscPayload
         , Arbitrary SscProof
         , Arbitrary SscPayloadDependsOnSlot
         , HasConfiguration
         ) =>
         Arbitrary T.MsgBlock where
    arbitrary = genericArbitrary
    shrink = genericShrink
