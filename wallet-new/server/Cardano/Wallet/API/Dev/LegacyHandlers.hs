{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module Cardano.Wallet.API.Dev.LegacyHandlers where

import           Universum

import qualified Cardano.Wallet.API.Dev as Dev
import qualified Cardano.Wallet.API.Dev.LegacyHandlers.Test as Test

import           Cardano.Wallet.API.V1.Migration

import           Servant

-- | Until we depend from V0 logic to implement the each 'Handler' we
-- still need the natural transformation here.
handlers :: ( HasConfigurations
            , HasCompileInfo
            )
            => (forall a. MonadV1 a -> Handler a)
            -> Server Dev.API
handlers naturalTransformation =
         hoistServer (Proxy @Dev.API) naturalTransformation Test.handlers
