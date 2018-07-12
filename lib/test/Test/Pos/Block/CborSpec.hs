{-# LANGUAGE TypeApplications #-}

module Test.Pos.Block.CborSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)

import           Pos.Core.Configuration (defaultCoreConfiguration,
                     withGenesisSpec)
import qualified Pos.Network.Block.Types as Block

import           Test.Pos.Binary.Helpers (binaryTest)
import           Test.Pos.Block.Arbitrary.Message ()
import           Test.Pos.Core.Arbitrary ()


spec :: Spec
spec =
    withGenesisSpec 0 defaultCoreConfiguration $ \_ ->
        describe "Block network types" $ modifyMaxSuccess (min 10) $ do
            binaryTest @Block.MsgGetHeaders
            binaryTest @Block.MsgGetBlocks
            binaryTest @Block.MsgHeaders
            binaryTest @Block.MsgBlock
            binaryTest @Block.MsgStream
            binaryTest @Block.MsgStreamBlock
