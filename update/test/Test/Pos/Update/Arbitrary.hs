{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-dodgy-exports    #-}
-- | Arbitrary instances for Update System types.

module Test.Pos.Update.Arbitrary
       ( module Test.Pos.Update.Arbitrary.Core
       , module Test.Pos.Update.Arbitrary.MemState
       , module Test.Pos.Update.Arbitrary.Network
       , module Test.Pos.Update.Arbitrary.Poll
       ) where

import           Test.Pos.Update.Arbitrary.Core
import           Test.Pos.Update.Arbitrary.MemState
import           Test.Pos.Update.Arbitrary.Network
import           Test.Pos.Update.Arbitrary.Poll
