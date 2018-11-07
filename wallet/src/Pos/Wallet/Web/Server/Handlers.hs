{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Wallet endpoints list

module Pos.Wallet.Web.Server.Handlers
       ( servantHandlers
       , servantHandlersWithSwagger
       ) where

import           Universum

import           Ntp.Client (NtpStatus)

import           Pos.Wallet.Web.Swagger.Spec (swaggerSpecForWalletApi)
import           Servant.API ((:<|>) ((:<|>)))
import           Servant.Server (Handler, Server, ServerT, hoistServer)
import           Servant.Server.Generic (genericServerT)
import           Servant.Swagger.UI (swaggerSchemaUIServer)

import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Txp (TxAux, TxpConfiguration)
import           Pos.Chain.Update (curSoftwareVersion)
import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Util.CompileInfo (HasCompileInfo)

import           Pos.Wallet.WalletMode (blockchainSlotDuration)
import           Pos.Wallet.Web.Account (GenSeed (RandomSeed))
import qualified Pos.Wallet.Web.Api as A
import qualified Pos.Wallet.Web.Methods as M
import           Pos.Wallet.Web.Mode (MonadFullWalletWebMode)

----------------------------------------------------------------------------
-- The wallet API with Swagger
----------------------------------------------------------------------------

servantHandlersWithSwagger
    :: ( MonadFullWalletWebMode ctx m
       , HasCompileInfo
       )
    => Genesis.Config
    -> TxpConfiguration
    -> TVar NtpStatus
    -> (TxAux -> m Bool)
    -> (forall x. m x -> Handler x)
    -> Server A.WalletSwaggerApi
servantHandlersWithSwagger genesisConfig txpConfig ntpStatus submitTx nat =
    hoistServer A.walletApi
                nat
                (servantHandlers genesisConfig txpConfig ntpStatus submitTx)
   :<|>
    swaggerSchemaUIServer swaggerSpecForWalletApi

----------------------------------------------------------------------------
-- The wallet API
----------------------------------------------------------------------------

servantHandlers
    :: ( MonadFullWalletWebMode ctx m
       , HasCompileInfo
       )
    => Genesis.Config
    -> TxpConfiguration
    -> TVar NtpStatus
    -> (TxAux -> m Bool)
    -> ServerT A.WalletApi m
servantHandlers genesisConfig txpConfig ntpStatus submitTx = do
    let nm = makeNetworkMagic $ configProtocolMagic genesisConfig
    genericServerT A.WalletApiRecord
        { _test        = testHandlers
        , _wallets     = walletsHandlers genesisConfig
        , _accounts    = accountsHandlers nm
        , _addresses   = addressesHandlers nm
        , _profile     = profileHandlers
        , _txs         = txsHandlers genesisConfig txpConfig submitTx
        , _update      = updateHandlers
        , _redemptions = redemptionsHandlers genesisConfig txpConfig submitTx
        , _reporting   = reportingHandlers
        , _settings    = settingsHandlers ntpStatus
        , _backup      = backupHandlers genesisConfig
        , _info        = infoHandlers
        , _system      = systemHandlers
        }

-- branches of the API

testHandlers :: MonadFullWalletWebMode ctx m => ServerT A.WTestApi m
testHandlers = genericServerT A.WTestApiRecord
    { _testReset = M.testResetAll
    , _testState = M.dumpState
    }

walletsHandlers
    :: MonadFullWalletWebMode ctx m => Genesis.Config -> ServerT A.WWalletsApi m
walletsHandlers genesisConfig = do
    let nm = makeNetworkMagic $ configProtocolMagic genesisConfig
    genericServerT A.WWalletsApiRecord
        { _getWallet              = M.getWallet nm
        , _getWallets             = M.getWallets nm
        , _newWallet              = M.newWallet nm
        , _updateWallet           = M.updateWallet nm
        , _restoreWallet          = M.restoreWalletFromSeed genesisConfig
        , _deleteWallet           = M.deleteWallet nm
        , _importWallet           = M.importWallet genesisConfig
        , _changeWalletPassphrase = M.changeWalletPassphrase nm
        }

accountsHandlers
    :: MonadFullWalletWebMode ctx m
    => NetworkMagic
    -> ServerT A.WAccountsApi m
accountsHandlers nm = genericServerT A.WAccountsApiRecord
    { _getAccount    = M.getAccount nm
    , _getAccounts   = M.getAccounts nm
    , _updateAccount = M.updateAccount nm
    , _newAccount    = M.newAccount nm RandomSeed
    , _deleteAccount = M.deleteAccount
    }

addressesHandlers
    :: MonadFullWalletWebMode ctx m
    => NetworkMagic
    -> ServerT A.WAddressesApi m
addressesHandlers nm = genericServerT A.WAddressesApiRecord
    { _newAddress     = M.newAddress nm RandomSeed
    , _isValidAddress = M.isValidAddress
    }

profileHandlers :: MonadFullWalletWebMode ctx m => ServerT A.WProfileApi m
profileHandlers = genericServerT A.WProfileApiRecord
    { _getProfile    = M.getUserProfile
    , _updateProfile = M.updateUserProfile
    }

txsHandlers
    :: MonadFullWalletWebMode ctx m
    => Genesis.Config
    -> TxpConfiguration
    -> (TxAux -> m Bool)
    -> ServerT A.WTxsApi m
txsHandlers genesisConfig txpConfig submitTx = genericServerT A.WTxsApiRecord
    { _newPayment                = M.newPayment genesisConfig txpConfig submitTx
    , _newPaymentBatch           = M.newPaymentBatch genesisConfig txpConfig submitTx
    , _txFee                     = M.getTxFee genesisConfig
    , _resetFailedPtxs           = M.resetAllFailedPtxs $
          configProtocolConstants genesisConfig
    , _cancelApplyingPtxs        = M.cancelAllApplyingPtxs
    , _cancelSpecificApplyingPtx = M.cancelOneApplyingPtx
    , _getHistory                = M.getHistoryLimited
    , _pendingSummary            = M.gatherPendingTxsSummary
    }

updateHandlers :: MonadFullWalletWebMode ctx m => ServerT A.WUpdateApi m
updateHandlers = genericServerT A.WUpdateApiRecord
    { _nextUpdate     = M.nextUpdate
    , _postponeUpdate = M.postponeUpdate
    , _applyUpdate    = M.applyUpdate
    }

redemptionsHandlers
    :: MonadFullWalletWebMode ctx m
    => Genesis.Config
    -> TxpConfiguration
    -> (TxAux -> m Bool)
    -> ServerT A.WRedemptionsApi m
redemptionsHandlers genesisConfig txpConfig submitTx = genericServerT A.WRedemptionsApiRecord
    { _redeemADA          = M.redeemAda genesisConfig txpConfig submitTx
    , _redeemADAPaperVend = M.redeemAdaPaperVend genesisConfig txpConfig submitTx
    }

reportingHandlers :: MonadFullWalletWebMode ctx m => ServerT A.WReportingApi m
reportingHandlers = genericServerT A.WReportingApiRecord
    { _reportingInitialized = M.reportingInitialized
    }

settingsHandlers :: MonadFullWalletWebMode ctx m => TVar NtpStatus -> ServerT A.WSettingsApi m
settingsHandlers ntpStatus = genericServerT A.WSettingsApiRecord
    { _getSlotsDuration    = blockchainSlotDuration <&> fromIntegral
    , _getVersion          = pure curSoftwareVersion
    , _getSyncProgress     = M.syncProgress
    , _localTimeDifference = fromMaybe 0 <$> M.localTimeDifference ntpStatus
    }

backupHandlers :: MonadFullWalletWebMode ctx m => Genesis.Config -> ServerT A.WBackupApi m
backupHandlers genesisConfig = genericServerT A.WBackupApiRecord
    { _importBackupJSON = M.importWalletJSON genesisConfig
    , _exportBackupJSON = M.exportWalletJSON genesisConfig
    }

infoHandlers :: (MonadFullWalletWebMode ctx m, HasCompileInfo) => ServerT A.WInfoApi m
infoHandlers = genericServerT A.WInfoApiRecord
    { _getClientInfo = M.getClientInfo
    }

systemHandlers :: MonadFullWalletWebMode ctx m => ServerT A.WSystemApi m
systemHandlers = genericServerT A.WSystemApiRecord
    { _requestShutdown = M.requestShutdown
    }
