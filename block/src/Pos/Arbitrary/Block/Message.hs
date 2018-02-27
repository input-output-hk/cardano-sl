
module Pos.Arbitrary.Block.Message
       (
       ) where

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Block ()
import           Pos.Arbitrary.Txp ()
import           Pos.Arbitrary.Update ()
import           Pos.Binary.Class (Bi, Raw)
import qualified Pos.Block.Network.Types as T
import           Pos.Core.Ssc (SscPayload, SscProof)
import           Pos.Crypto (ProtocolMagic)

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
         , Arbitrary ProtocolMagic
         ) =>
         Arbitrary T.MsgHeaders where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance ( Arbitrary SscPayload
         , Arbitrary SscProof
         , Arbitrary ProtocolMagic
         ) =>
         Arbitrary T.MsgBlock where
    arbitrary = genericArbitrary
    shrink = genericShrink
