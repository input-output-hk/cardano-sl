-- | Some hacks we use in Auxx.

module Pos.Auxx.Hacks
       ( makePubKeyAddressAuxx
       ) where

import           Universum

import           Pos.Core   (Address, IsBootstrapEraAddr (..), makePubKeyAddress)
import           Pos.Binary.Core.Address ()
import           Pos.Crypto (PublicKey)
import           Pos.DB     (MonadGState, gsIsBootstrapEra)

-- | In order to create an 'Address' from a 'PublicKey' we need to
-- choose suitable stake distribution. We want to pick it based on
-- whether we are currently in bootstrap era.  Auxx doesn't know
-- current slot (actually it does, but we are not using it yet), so
-- let's assume it's 0-th epoch. It's enough for our current needs.
makePubKeyAddressAuxx :: MonadGState m => PublicKey -> m Address
makePubKeyAddressAuxx pk = do
    ibea <- IsBootstrapEraAddr <$> gsIsBootstrapEra 0
    return $ makePubKeyAddress ibea pk
