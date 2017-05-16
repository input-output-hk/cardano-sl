-- | Different utils for wallets

module Pos.Wallet.Web.Util
    ( deriveLvl2KeyPair
    ) where

import           Universum

import           Pos.Core   (Address, createHDAddressH)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase, deriveHDPassphrase,
                             deriveHDSecretKey, encToPublic)

-- TODO: move more here from Methods.hs

-- | Makes account secret key for given wallet set.
deriveLvl2KeyPair
    :: PassPhrase
    -> EncryptedSecretKey  -- ^ key of wallet set
    -> Word32              -- ^ wallet derivation index
    -> Word32              -- ^ account derivation index
    -> Maybe (Address, EncryptedSecretKey)
deriveLvl2KeyPair passphrase wsKey walletIndex accIndex = do
    wKey <- deriveHDSecretKey passphrase wsKey walletIndex
    let hdPass = deriveHDPassphrase $ encToPublic wsKey
    createHDAddressH passphrase hdPass wKey [walletIndex] accIndex
