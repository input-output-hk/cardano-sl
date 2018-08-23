{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.Wallet.API.Internal.LegacyHandlers where

import           Universum

import           Control.Monad.Catch (MonadThrow)

import           Servant

import qualified Pos.Client.KeyStorage as V0
import           Pos.Core.Update (SoftwareVersion)
import qualified Pos.Wallet.Web.ClientTypes as V0
import qualified Pos.Wallet.Web.Methods.Misc as V0
import qualified Pos.Wallet.Web.State as V0

import qualified Cardano.Wallet.API.Internal as Internal
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types (V1 (..))
import           Cardano.Wallet.Server.CLI (RunMode (..), isDebugMode)

-- | Until we depend from V0 logic to implement the each 'Handler' we
-- still need the natural transformation here.
handlers
    :: (HasConfiguration, HasUpdateConfiguration)
    => (forall a. MonadV1 a -> Handler a)
    -> RunMode
    -> Server Internal.API
handlers naturalTransformation runMode =
    let
        handlers' =
                 nextUpdate
            :<|> V0.applyUpdate
            :<|> V0.postponeUpdate
            :<|> resetWalletState runMode
    in
        hoistServer (Proxy @Internal.API) naturalTransformation handlers'

nextUpdate :: ( MonadIO m
              , HasConfiguration
              , MonadThrow m
              , V0.WalletDbReader ctx m
              , HasUpdateConfiguration
              )
           => m (V1 SoftwareVersion)
nextUpdate = (V1 . V0.cuiSoftwareVersion) <$> V0.nextUpdate

resetWalletState
    :: (V0.WalletDbReader ctx m, V0.MonadKeys m, MonadThrow m, MonadIO m)
    => RunMode
    -> m NoContent
resetWalletState runMode
    | isDebugMode runMode = do
        V0.deleteAllSecretKeys
        void (V0.askWalletDB >>= V0.testReset)
        return NoContent
    | otherwise =
        throwM err403
