-- | Some hacks we use in Rubbish.

module Pos.Rubbish.Hacks
       ( makePubKeyAddressRubbish
       ) where

import           Universum

import           Pos.Core   (Address, IsBootstrapEraAddr (..), makePubKeyAddress)
import           Pos.Crypto (PublicKey)
import           Pos.DB     (MonadGState, gsIsBootstrapEra)

-- | In order to create an 'Address' from a 'PublicKey' we need to
-- choose suitable stake distribution. We want to pick it based on
-- whether we are currently in bootstrap era.  Light wallet doesn't
-- know current slot, so let's assume it's 0-th epoch. It's enough for
-- our current needs.
makePubKeyAddressRubbish :: MonadGState m => PublicKey -> m Address
makePubKeyAddressRubbish pk = do
    ibea <- IsBootstrapEraAddr <$> gsIsBootstrapEra 0
    return $ makePubKeyAddress ibea pk
