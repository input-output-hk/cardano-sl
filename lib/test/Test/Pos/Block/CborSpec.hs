{-# LANGUAGE TypeApplications #-}

module Test.Pos.Block.CborSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)

import qualified Pos.Chain.Block as Block
import qualified Pos.Network.Block.Types as Block

import           Test.Pos.Binary.Helpers (binaryTest)
import           Test.Pos.Block.Arbitrary.Message ()
import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.DB.Block.Arbitrary ()

spec :: Spec
spec = do
        describe "Block network types" $ modifyMaxSuccess (min 10) $ do
            binaryTest @Block.MsgGetHeaders
            binaryTest @Block.MsgGetBlocks
            binaryTest @Block.MsgHeaders
            binaryTest @Block.MsgBlock
            binaryTest @Block.MsgStream
            binaryTest @Block.MsgStreamBlock

        describe "Block types defined in the block package" $ do
            describe "Bi instances" $ do
                describe "Undo" $ do
                    binaryTest @Block.SlogUndo
                    modifyMaxSuccess (min 50) $ do
                        binaryTest @Block.Undo
