{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.Block.Arbitrary.Message
       (
       ) where

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import qualified Pos.Network.Block.Types as T

import           Test.Pos.Chain.Block.Arbitrary ()
import           Test.Pos.Core.Chrono ()
-- import           Test.Pos.Chain.Ssc.Arbitrary ()
import           Test.Pos.Chain.Update.Arbitrary ()

------------------------------------------------------------------------------------------
-- Block network types
------------------------------------------------------------------------------------------

instance Arbitrary T.MsgGetHeaders where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.MsgGetBlocks where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.MsgHeaders where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.MsgBlock where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.MsgStream where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.MsgStreamStart where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.MsgStreamUpdate where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.MsgStreamBlock where
    arbitrary = genericArbitrary
    shrink = genericShrink
