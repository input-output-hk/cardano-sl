{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.Wallet.API.Development.LegacyHandlers
    ( handlers
    , deleteSecretKeys
    ) where

import           Universum
import           Control.Monad.Catch (MonadThrow)

import qualified Cardano.Wallet.API.Development as Dev
import           Cardano.Wallet.API.Development.Helpers (developmentOnly)
import           Cardano.Wallet.API.Response (WalletResponse, single)
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types (V1(..))
import           Cardano.Wallet.Server.CLI (RunMode (..))

import qualified Pos.Core as V0
import qualified Pos.Configuration as V0
import qualified Pos.Wallet.Web.State as V0
import qualified Pos.Client.KeyStorage as V0
import qualified Pos.Wallet.Web.Methods.Misc as V0

import           Servant

-- | Until we depend from V0 logic to implement the each 'Handler' we
-- still need the natural transformation here.
handlers :: ( HasConfigurations
            , HasCompileInfo
            )
         => (forall a. MonadV1 a -> Handler a)
         -> RunMode
         -> Server Dev.API
handlers naturalTransformation runMode =
         hoistServer (Proxy @Dev.API) naturalTransformation handlers'
         where
            -- | @Servant@ handlers needed by test cases.
            -- They are not available in public, but for development mode only.
            handlers' :: ( HasCompileInfo
                         , HasConfigurations
                         , V0.WalletDbReader ctx m
                         , V0.MonadKeys m
                         , MonadThrow m
                         , MonadIO m
                         )
                      => ServerT Dev.API m
            handlers' =    getWalletState runMode
                      :<|> deleteSecretKeys runMode
                      :<|> throwSomething runMode

getWalletState :: ( MonadThrow m, MonadIO m, V0.WalletDbReader ctx m)
               => RunMode
               -> m (WalletResponse (V1 V0.WalletStateSnapshot))
getWalletState runMode =
    developmentOnly runMode (fmap V1 . single <$> V0.dumpState)

deleteSecretKeys :: ( V0.HasNodeConfiguration
                    , V0.HasConfiguration
                    , V0.WalletDbReader ctx m
                    , V0.MonadKeys m
                    , MonadThrow m
                    , MonadIO m
                    )
                 => RunMode
                 -> m NoContent
deleteSecretKeys runMode = do
    db <- V0.askWalletDB
    developmentOnly runMode (V0.deleteAllSecretKeys >> V0.testReset db >> return NoContent)

throwSomething :: MonadThrow m => RunMode -> m NoContent
throwSomething runMode =
    developmentOnly runMode $ error "A generic error happened in dev mode"
