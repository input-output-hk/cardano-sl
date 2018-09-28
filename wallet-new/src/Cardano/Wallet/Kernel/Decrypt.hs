module Cardano.Wallet.Kernel.Decrypt
    ( decryptAddress
    , decryptHdLvl2DerivationPath
    , eskToWalletDecrCredentials
    , selectOwnAddresses
    , WalletDecrCredentials
    ) where

import           Universum

import           Formatting (build, sformat)

import           Cardano.Wallet.API.V1.Types as V1
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountIx (..),
                     HdAddressIx (..))
import           Pos.Core (aaPkDerivationPath, addrAttributesUnwrapped,
                     makeRootPubKeyAddress)
import           Pos.Core.NetworkMagic (NetworkMagic)
import           Pos.Crypto (EncryptedSecretKey, HDPassphrase, PublicKey,
                     deriveHDPassphrase, encToPublic, unpackHDAddressAttr)


type WalletDecrCredentials = (HDPassphrase, V1.WalletId)

decryptHdLvl2DerivationPath :: HDPassphrase
                            -> Address
                            -> Maybe (HdAccountIx, HdAddressIx)
decryptHdLvl2DerivationPath hdPass addr = do
    hdPayload <- aaPkDerivationPath $ addrAttributesUnwrapped addr
    derPath <- unpackHDAddressAttr hdPass hdPayload
    case derPath of
        [a,b] -> Just (HdAccountIx a, HdAddressIx b)
        _     -> Nothing

-- | There's a secret key for regular wallet or a public key for external wallet.
eskToWalletDecrCredentials :: NetworkMagic -> EncryptedSecretKey -> WalletDecrCredentials
eskToWalletDecrCredentials nm esk = credentialsFromPublicKey nm $ encToPublic esk

credentialsFromPublicKey :: NetworkMagic -> PublicKey -> WalletDecrCredentials
credentialsFromPublicKey nm publicKey = (hdPassword, walletId)
  where
    hdPassword = deriveHDPassphrase publicKey

    -- When migrating from 'Pos.Wallet.Web.Tracking.Decrypt' this type is
    -- changed from 'CId Wal' to 'V1.WalletId'
    walletId   = V1.WalletId . (sformat build) $ makeRootPubKeyAddress nm publicKey

selectOwnAddresses
    :: WalletDecrCredentials
    -> (a -> Address)
    -> [a]
    -> [(a, V1.WAddressMeta)]
selectOwnAddresses wdc getAddr =
    mapMaybe (\a -> (a,) <$> decryptAddress wdc (getAddr a))

decryptAddress :: WalletDecrCredentials -> Address -> Maybe V1.WAddressMeta
decryptAddress (hdPass, wCId) addr = do
    hdPayload <- aaPkDerivationPath $ addrAttributesUnwrapped addr
    derPath <- unpackHDAddressAttr hdPass hdPayload
    case derPath of
        [a,b] -> Just $ WAddressMeta wCId a b (V1 addr)
        _     -> Nothing
