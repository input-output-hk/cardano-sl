{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TemplateHaskell           #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Test.Pos.Cbor.CborSpec specification

module Test.Pos.Crypto.CborSpec
       ( spec
       ) where

import           Universum

import           Crypto.Hash (Blake2b_224, Blake2b_256)
import           Test.Hspec (Spec, describe)

import           Pos.Binary.Class
import qualified Pos.Crypto as Crypto

import           Test.Pos.Binary.Helpers (U, binaryTest)
import           Test.Pos.Crypto.Arbitrary ()



spec :: Spec
spec =
    describe "Cbor.Bi instances" $ do
        describe "Crypto" $ do
            describe "Hashing" $ do
                binaryTest @(Crypto.Hash Word64)
            describe "Signing" $ do
                describe "Bi instances" $ do
                    binaryTest @Crypto.SecretKey
                    binaryTest @Crypto.PublicKey
                    binaryTest @(Crypto.Signature ())
                    binaryTest @(Crypto.Signature U)
                    binaryTest @(Crypto.ProxyCert Int32)
                    binaryTest @(Crypto.ProxySecretKey Int32)
                    binaryTest @(Crypto.ProxySecretKey U)
                    binaryTest @(Crypto.ProxySignature Int32 Int32)
                    binaryTest @(Crypto.ProxySignature U U)
                    binaryTest @(Crypto.Signed Bool)
                    binaryTest @(Crypto.Signed U)
                    binaryTest @Crypto.RedeemSecretKey
                    binaryTest @Crypto.RedeemPublicKey
                    binaryTest @(Crypto.RedeemSignature Bool)
                    binaryTest @(Crypto.RedeemSignature U)
                    binaryTest @Crypto.Threshold
                    binaryTest @Crypto.VssPublicKey
                    binaryTest @Crypto.PassPhrase
                    binaryTest @Crypto.VssKeyPair
                    binaryTest @Crypto.Secret
                    binaryTest @Crypto.DecShare
                    binaryTest @Crypto.EncShare
                    binaryTest @Crypto.SecretProof
                    binaryTest @Crypto.HDAddressPayload
                    binaryTest @(Crypto.AbstractHash Blake2b_224 U)
                    binaryTest @(Crypto.AbstractHash Blake2b_256 U)
                    binaryTest @(AsBinary Crypto.VssPublicKey)
                    binaryTest @(AsBinary Crypto.Secret)
                    binaryTest @(AsBinary Crypto.DecShare)
                    binaryTest @(AsBinary Crypto.EncShare)
