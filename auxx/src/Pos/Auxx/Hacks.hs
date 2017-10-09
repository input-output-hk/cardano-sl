-- | Some hacks we use in Auxx.

module Pos.Auxx.Hacks
       ( makePubKeyAddressAuxx
       , deriveHDAddressAuxx
       ) where

import           Universum

import           Pos.Binary.Core.Address ()
import           Pos.Core                (Address, IsBootstrapEraAddr (..),
                                          makePubKeyAddress)
import           Pos.Core.Address        (deriveFirstHDAddress)
import           Pos.Crypto              (EncryptedSecretKey, PublicKey, emptyPassphrase)
import           Pos.DB                  (MonadGState, gsIsBootstrapEra)

-- | In order to create an 'Address' from a 'PublicKey' we need to
-- choose suitable stake distribution. We want to pick it based on
-- whether we are currently in bootstrap era.  Auxx doesn't know
-- current slot (actually it does, but we are not using it yet), so
-- let's assume it's 0-th epoch. It's enough for our current needs.
makePubKeyAddressAuxx :: MonadGState m => PublicKey -> m Address
makePubKeyAddressAuxx pk = do
    ibea <- IsBootstrapEraAddr <$> gsIsBootstrapEra 0
    return $ makePubKeyAddress ibea pk

-- | Similar to @makePubKeyAddressAuxx@ but create HD public key.
deriveHDAddressAuxx :: MonadGState m => EncryptedSecretKey -> m Address
deriveHDAddressAuxx hdwSk = do
    ibea <- IsBootstrapEraAddr <$> gsIsBootstrapEra 0
    pure $ fst $ fromMaybe (error "makePubKeyHDAddressAuxx: pass mismatch") $
        deriveFirstHDAddress ibea emptyPassphrase hdwSk
