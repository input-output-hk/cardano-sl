{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.Wallet.API.Internal.Handlers where

import           Universum

import           Control.Monad.Catch (MonadThrow)
import           Servant

import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.Server.CLI (RunMode (..), isDebugMode)

import qualified Cardano.Wallet.API.Internal as Internal
import qualified Pos.Client.KeyStorage as V0
import           Pos.Util.Trace.Named (TraceNamed)
import qualified Pos.Wallet.Web.Methods.Misc as V0
import qualified Pos.Wallet.Web.State as V0

-- | Until we depend from V0 logic to implement the each 'Handler' we
-- still need the natural transformation here.
handlers
    :: TraceNamed MonadV1
    -> (forall a. MonadV1 a -> Handler a)
    -> RunMode
    -> Server Internal.API
handlers logTrace naturalTransformation runMode =
    let
        handlers' =
                 V0.applyUpdate logTrace
            :<|> V0.postponeUpdate
            :<|> resetWalletState runMode
    in
        hoistServer (Proxy @Internal.API) naturalTransformation handlers'


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
