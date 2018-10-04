{-# LANGUAGE TypeApplications #-}

module Test.Pos.Chain.Update.CborSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)

import           Pos.Chain.Update (ApplicationName (..), BlockVersion (..),
                     BlockVersionData (..), SoftforkRule (..),
                     SoftwareVersion (..))

import           Test.Pos.Binary.Helpers (binaryTest)
import           Test.Pos.Chain.Update.Arbitrary ()

spec :: Spec
spec = describe "Cbor Bi instances" $ do
    binaryTest @ApplicationName
    binaryTest @BlockVersion
    binaryTest @BlockVersionData
    binaryTest @SoftforkRule
    binaryTest @SoftwareVersion
