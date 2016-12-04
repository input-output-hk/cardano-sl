{-# LANGUAGE TypeApplications #-}

-- | This module tests MessagePack instances.

module Test.Pos.Ssc.GodTossing.Identity.MessagePackSpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import qualified Pos.Ssc.GodTossing  as GT

import           Test.Pos.Util         (msgPackEncodeDecode)

spec :: Spec
spec = describe "Types" $ do
    describe "MessagePack instances" $ do
        prop "CommitmentSignature" (msgPackEncodeDecode @GT.CommitmentSignature)
