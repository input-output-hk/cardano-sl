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
import           UnliftIO (MonadUnliftIO)

import qualified Pos.Client.KeyStorage as V0
import           Pos.Core as Core (Config)
import           Pos.Core.Update (SoftwareVersion)
import           Pos.Crypto (emptyPassphrase)
import           Pos.Util (HasLens (..))
import qualified Pos.Wallet.WalletMode as V0
import qualified Pos.Wallet.Web.ClientTypes as V0
import qualified Pos.Wallet.Web.Methods as V0
import qualified Pos.Wallet.Web.State as V0
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)

import qualified Cardano.Wallet.API.Internal as Internal
import           Cardano.Wallet.API.Response (WalletResponse, single)
import qualified Cardano.Wallet.API.V1.LegacyHandlers.Wallets as Legacy
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types (V1 (..), Wallet,
                     WalletImport (..), unV1)
import           Cardano.Wallet.Server.CLI (RunMode (..), isDebugMode)

-- | Until we depend from V0 logic to implement the each 'Handler' we
-- still need the natural transformation here.
handlers
    :: HasUpdateConfiguration
    => (forall a. MonadV1 a -> Handler a)
    -> Core.Config
    -> RunMode
    -> Server Internal.API
handlers naturalTransformation coreConfig runMode =
    let
        handlers' =
                 nextUpdate
            :<|> V0.applyUpdate
            :<|> V0.postponeUpdate
            :<|> resetWalletState runMode
            :<|> importWallet coreConfig
    in
        hoistServer (Proxy @Internal.API) naturalTransformation handlers'

nextUpdate :: ( MonadIO m
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

importWallet
    :: ( V0.MonadWalletLogic ctx m
       , V0.MonadBlockchainInfo m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       )
    => Core.Config
    -> WalletImport
    -> m (WalletResponse Wallet)
importWallet coreConfig WalletImport{..} = do
    fp <- migrate wiFilePath
    v0Wallet <- V0.importWallet coreConfig (maybe emptyPassphrase unV1 wiSpendingPassword) fp
    snapshot <- V0.askWalletSnapshot
    single <$> Legacy.addWalletInfo snapshot v0Wallet
