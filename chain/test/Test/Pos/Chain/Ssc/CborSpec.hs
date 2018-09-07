{-# LANGUAGE TypeApplications #-}

module Test.Pos.Chain.Ssc.CborSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)

import           Pos.Chain.Ssc (VssCertificate)

import           Test.Pos.Binary.Helpers (binaryTest)
import           Test.Pos.Chain.Ssc.Arbitrary ()

spec :: Spec
spec = describe "Cbor Bi instances" $ binaryTest @VssCertificate
