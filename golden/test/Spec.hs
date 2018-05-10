
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Inspector

import Foundation
import Foundation.Check
import Basement.Block (Block)
import Basement.Sized.List (ListN, unListN)

-------- test imports
import           Control.Monad (replicateM)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteArray as BA

import Pos.Core.Common
import Pos.Crypto.Signing
import Pos.Crypto.HD
import Pos.Data.Attributes
import qualified Cardano.Crypto.Wallet as CC

import GHC.Generics
import Pos.Binary.Class
import Pos.Binary.Core
import Pos.Core.Common (AddrAttributes (..), AddrSpendingData (..),
                        AddrStakeDistribution (..), AddrType (..), Address (..),
                        Address' (..), AddressHash, Script, StakeholderId, IsBootstrapEraAddr(..))

type GoldenAddressPrime =
    "AddressPrime" :> Payload "root-publickey" PublicKey
                   :> Payload "derived-publickey" PublicKey
                   :> Payload "derivation-path" [Word32]
                   :> Payload "address-prime" (Block Word8)

type GoldenAddress =
    "Address" :> Payload "root-publickey" PublicKey
              :> Payload "derived-publickey" PublicKey
              :> Payload "derivation-path" [Word32]
              :> Payload "address" (Block Word8)

dumpBi :: Bi a => a -> Block Word8
dumpBi a = BA.convert $ BSL.toStrict (serialize a)

instance Arbitrary PublicKey where
    arbitrary = either (error . fromList) PublicKey . CC.xpub . BA.pack . unListN <$> arbitrary @(ListN 64 Word8)
    
instance Inspectable PublicKey where
    documentation _ = "wallet public key"
    exportType _ t = exportType (Proxy :: Proxy [Word8]) t
    parser _ = either (error . fromList) PublicKey . CC.xpub . BA.convert
           <$> parser (Proxy @(Block Word8))
    display ty (PublicKey pub) = display ty (BA.convert (CC.unXPub pub) :: Block Word8)

main :: IO ()
main = defaultMain $ do
    golden (Proxy @GoldenAddressPrime) $ \rootPub derivePub derivePath ->
        let
            hdPassPhrase     = deriveHDPassphrase rootPub
            hdAddressPayload = packHDAddressAttr hdPassPhrase derivePath

            bootstrapEra     = IsBootstrapEraAddr True
            spendingData     = PubKeyASD derivePub
            aa               = AddrAttributes (Just hdAddressPayload) BootstrapEraDistr
            attributes       = mkAttributes aa

            addr'            = Address' (ATPubKey, spendingData, attributes)
         in dumpBi addr'
    golden (Proxy @GoldenAddress) $ \rootPub derivePub derivePath ->
        let
            hdPassPhrase     = deriveHDPassphrase rootPub
            hdAddressPayload = packHDAddressAttr hdPassPhrase derivePath

            bootstrapEra     = IsBootstrapEraAddr True
            spendingData     = PubKeyASD derivePub
            aa               = AddrAttributes (Just hdAddressPayload) BootstrapEraDistr
            attributes       = mkAttributes aa
            addr             = makePubKeyHdwAddress bootstrapEra hdAddressPayload rootPub
         in dumpBi addr
