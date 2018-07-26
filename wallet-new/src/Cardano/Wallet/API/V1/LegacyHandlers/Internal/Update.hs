module Cardano.Wallet.API.V1.LegacyHandlers.Internal.Update where

import           Servant

import qualified Pos.Wallet.Web.Methods.Misc as V0

import qualified Cardano.Wallet.API.V1.Internal.Update as Update
import           Cardano.Wallet.API.V1.Migration (MonadV1)

handlers :: ServerT Update.API MonadV1
handlers = applyUpdate :<|> postponeUpdate

applyUpdate :: MonadV1 NoContent
applyUpdate = V0.applyUpdate

postponeUpdate :: MonadV1 NoContent
postponeUpdate = V0.postponeUpdate
