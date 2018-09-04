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
import           Servant.Generic (AsServerT, GenericProduct, ToServant, toServant)
import           Servant.Server (Handler, Server, ServerT, hoistServer)
import           Servant.Swagger.UI (swaggerSchemaUIServer)

import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Core.Txp (TxAux)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Update.Configuration (curSoftwareVersion)
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
    => ProtocolMagic
    -> TVar NtpStatus
    -> (TxAux -> m Bool)
    -> (forall x. m x -> Handler x)
    -> Server A.WalletSwaggerApi
servantHandlersWithSwagger pm ntpStatus submitTx nat =
    hoistServer A.walletApi nat (servantHandlers pm ntpStatus submitTx)
   :<|>
    swaggerSchemaUIServer swaggerSpecForWalletApi

----------------------------------------------------------------------------
-- The wallet API
----------------------------------------------------------------------------

servantHandlers
    :: ( MonadFullWalletWebMode ctx m
       , HasCompileInfo
       )
    => ProtocolMagic
    -> TVar NtpStatus
    -> (TxAux -> m Bool)
    -> ServerT A.WalletApi m
servantHandlers pm ntpStatus submitTx =
    let nm = makeNetworkMagic pm in
    toServant' A.WalletApiRecord
    { _test        = testHandlers
    , _wallets     = walletsHandlers nm
    , _accounts    = accountsHandlers nm
    , _addresses   = addressesHandlers nm
    , _profile     = profileHandlers
    , _txs         = txsHandlers pm submitTx
    , _update      = updateHandlers
    , _redemptions = redemptionsHandlers pm submitTx
    , _reporting   = reportingHandlers
    , _settings    = settingsHandlers ntpStatus
    , _backup      = backupHandlers nm
    , _info        = infoHandlers
    , _system      = systemHandlers
    }

-- branches of the API

testHandlers :: MonadFullWalletWebMode ctx m => ServerT A.WTestApi m
testHandlers = toServant' A.WTestApiRecord
    { _testReset = M.testResetAll
    , _testState = M.dumpState
    }

walletsHandlers
    :: MonadFullWalletWebMode ctx m
    => NetworkMagic
    -> ServerT A.WWalletsApi m
walletsHandlers nm = toServant' A.WWalletsApiRecord
    { _getWallet              = M.getWallet nm
    , _getWallets             = M.getWallets nm
    , _newWallet              = M.newWallet nm
    , _updateWallet           = M.updateWallet nm
    , _restoreWallet          = M.restoreWalletFromSeed nm
    , _deleteWallet           = M.deleteWallet nm
    , _importWallet           = M.importWallet nm
    , _changeWalletPassphrase = M.changeWalletPassphrase nm
    }

accountsHandlers
    :: MonadFullWalletWebMode ctx m
    => NetworkMagic
    -> ServerT A.WAccountsApi m
accountsHandlers nm = toServant' A.WAccountsApiRecord
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
addressesHandlers nm = toServant' A.WAddressesApiRecord
    { _newAddress     = M.newAddress nm RandomSeed
    , _isValidAddress = M.isValidAddress
    }

profileHandlers :: MonadFullWalletWebMode ctx m => ServerT A.WProfileApi m
profileHandlers = toServant' A.WProfileApiRecord
    { _getProfile    = M.getUserProfile
    , _updateProfile = M.updateUserProfile
    }

txsHandlers
    :: MonadFullWalletWebMode ctx m
    => ProtocolMagic
    -> (TxAux -> m Bool)
    -> ServerT A.WTxsApi m
txsHandlers pm submitTx = toServant' A.WTxsApiRecord
    { _newPayment                = M.newPayment pm submitTx
    , _newPaymentBatch           = M.newPaymentBatch pm submitTx
    , _txFee                     = M.getTxFee pm
    , _resetFailedPtxs           = M.resetAllFailedPtxs
    , _cancelApplyingPtxs        = M.cancelAllApplyingPtxs
    , _cancelSpecificApplyingPtx = M.cancelOneApplyingPtx
    , _getHistory                = M.getHistoryLimited
    , _pendingSummary            = M.gatherPendingTxsSummary
    }

updateHandlers :: MonadFullWalletWebMode ctx m => ServerT A.WUpdateApi m
updateHandlers = toServant' A.WUpdateApiRecord
    { _nextUpdate     = M.nextUpdate
    , _postponeUpdate = M.postponeUpdate
    , _applyUpdate    = M.applyUpdate
    }

redemptionsHandlers
    :: MonadFullWalletWebMode ctx m
    => ProtocolMagic
    -> (TxAux -> m Bool)
    -> ServerT A.WRedemptionsApi m
redemptionsHandlers pm submitTx = toServant' A.WRedemptionsApiRecord
    { _redeemADA          = M.redeemAda pm submitTx
    , _redeemADAPaperVend = M.redeemAdaPaperVend pm submitTx
    }

reportingHandlers :: MonadFullWalletWebMode ctx m => ServerT A.WReportingApi m
reportingHandlers = toServant' A.WReportingApiRecord
    { _reportingInitialized = M.reportingInitialized
    }

settingsHandlers :: MonadFullWalletWebMode ctx m => TVar NtpStatus -> ServerT A.WSettingsApi m
settingsHandlers ntpStatus = toServant' A.WSettingsApiRecord
    { _getSlotsDuration    = blockchainSlotDuration <&> fromIntegral
    , _getVersion          = pure curSoftwareVersion
    , _getSyncProgress     = M.syncProgress
    , _localTimeDifference = fromMaybe 0 <$> M.localTimeDifference ntpStatus
    }

backupHandlers
    :: MonadFullWalletWebMode ctx m
    => NetworkMagic
    -> ServerT A.WBackupApi m
backupHandlers nm = toServant' A.WBackupApiRecord
    { _importBackupJSON = M.importWalletJSON nm
    , _exportBackupJSON = M.exportWalletJSON nm
    }

infoHandlers :: (MonadFullWalletWebMode ctx m, HasCompileInfo) => ServerT A.WInfoApi m
infoHandlers = toServant' A.WInfoApiRecord
    { _getClientInfo = M.getClientInfo
    }

systemHandlers :: MonadFullWalletWebMode ctx m => ServerT A.WSystemApi m
systemHandlers = toServant' A.WSystemApiRecord
    { _requestShutdown = M.requestShutdown
    }

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

-- | A type-restricted synonym for 'toServant' that lets us avoid some type
-- annotations
toServant'
    :: (a ~ r (AsServerT m), GenericProduct a)
    => a -> ToServant a
toServant' = toServant
