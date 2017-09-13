-- | Some hacks we use in light wallet.

module Pos.Wallet.Light.Hacks
       ( makePubKeyAddressLWallet
       ) where

import           Universum

import           Pos.Core   (Address, IsBootstrapEraAddr (..), makePubKeyAddress)
import           Pos.Binary.Core.Address ()
import           Pos.Crypto (PublicKey)
import           Pos.DB     (MonadGState, gsIsBootstrapEra)

-- | In order to create an 'Address' from a 'PublicKey' we need to
-- choose suitable stake distribution. We want to pick it based on
-- whether we are currently in bootstrap era.  Light wallet doesn't
-- know current slot, so let's assume it's 0-th epoch. It's enough for
-- our current needs.
makePubKeyAddressLWallet :: MonadGState m => PublicKey -> m Address
makePubKeyAddressLWallet pk = do
    ibea <- IsBootstrapEraAddr <$> gsIsBootstrapEra 0
    return $ makePubKeyAddress ibea pk
