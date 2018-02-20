{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.Wallet.API.Development.LegacyHandlers
    ( handlers
    ) where

import           Universum

import qualified Cardano.Wallet.API.Development as Dev
import           Cardano.Wallet.API.Response (WalletResponse, single)
import           Cardano.Wallet.API.V1.Migration

import           Pos.Wallet.Web.State as V0
import qualified Pos.Wallet.Web.Methods.Misc as V0

import           Servant


-- | Until we depend from V0 logic to implement the each 'Handler' we
-- still need the natural transformation here.
handlers :: ( HasConfigurations
            , HasCompileInfo
            )
            => (forall a. MonadV1 a -> Handler a)
            -> Server Dev.API
handlers naturalTransformation =
         hoistServer (Proxy @Dev.API) naturalTransformation handlers'
         where
            -- | @Servant@ handlers needed by test cases.
            -- They are not available in public, but for development mode only.
            handlers' :: ( HasConfigurations
                         , HasCompileInfo
                         )
                         => ServerT Dev.API MonadV1
            handlers' = getWalletState


getWalletState :: (V0.MonadWalletDBRead ctx m)
   => m (WalletResponse V0.WalletStateSnapshot)
getWalletState =
   single <$> V0.dumpState
