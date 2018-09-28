module Cardano.Wallet.Kernel.Decrypt
    ( decryptAddress
    , decryptHdLvl2DerivationPath
    , keyToWalletDecrCredentials
    , selectOwnAddresses
    , WalletDecrCredentials
    , WalletDecrCredentialsKey (..)
    ) where

import           Universum

import           Formatting (build, sformat)

import           Cardano.Wallet.API.V1.Types as V1
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountIx (..),
                     HdAddressIx (..))
import           Pos.Core (aaPkDerivationPath, addrAttributesUnwrapped,
                     makeRootPubKeyAddress)
import           Pos.Crypto (EncryptedSecretKey, HDPassphrase, PublicKey,
                     deriveHDPassphrase, encToPublic, unpackHDAddressAttr)

decryptHdLvl2DerivationPath :: HDPassphrase
                            -> Address
                            -> Maybe (HdAccountIx, HdAddressIx)
decryptHdLvl2DerivationPath hdPass addr = do
    hdPayload <- aaPkDerivationPath $ addrAttributesUnwrapped addr
    derPath <- unpackHDAddressAttr hdPass hdPayload
    guard $ length derPath == 2
    pure (HdAccountIx (derPath !! 0), HdAddressIx (derPath !! 1))


type WalletDecrCredentials = (HDPassphrase, V1.WalletId)

-- | Key to identify regular or external wallet.
data WalletDecrCredentialsKey
    = KeyForRegular EncryptedSecretKey
    | KeyForExternal PublicKey
    deriving (Show)

-- | There's a secret key for regular wallet or a public key for external wallet.
keyToWalletDecrCredentials :: WalletDecrCredentialsKey -> WalletDecrCredentials
keyToWalletDecrCredentials (KeyForRegular sk)  = credentialsFromPublicKey $ encToPublic sk
keyToWalletDecrCredentials (KeyForExternal pk) = credentialsFromPublicKey pk

credentialsFromPublicKey :: PublicKey -> WalletDecrCredentials
credentialsFromPublicKey publicKey = (hdPassword, walletId)
  where
    hdPassword = deriveHDPassphrase publicKey

    -- When migrating from 'Pos.Wallet.Web.Tracking.Decrypt' this type is
    -- changed from 'CId Wal' to 'V1.WalletId'
    walletId   = V1.WalletId . (sformat build) $ makeRootPubKeyAddress publicKey

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
    guard $ length derPath == 2
    pure $ WAddressMeta wCId (derPath !! 0) (derPath !! 1) (V1 addr)
