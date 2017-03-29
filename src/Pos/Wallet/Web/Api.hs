{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

-- | Servant API for wallet.

module Pos.Wallet.Web.Api
       ( WalletApi
       , walletApi
       ) where


import           Servant.API                ((:<|>), (:>), Capture, Delete, Get, JSON,
                                             Post, Put, QueryParam, ReqBody)
import           Universum

import           Pos.Types                  (Coin, SoftwareVersion)
import           Pos.Wallet.Web.ClientTypes (CAccount, CAccountAddress, CCurrency,
                                             CInitialized, CPassPhrase, CProfile, CTx,
                                             CTxId, CTxMeta, CUpdateInfo, CWallet,
                                             CWalletAddress, CWalletInit, CWalletMeta,
                                             CWalletRedeem, CWalletSet, CWalletSet,
                                             CWalletSetAddress, CWalletSetInit,
                                             SyncProgress)
import           Pos.Wallet.Web.Error       (WalletError)


-- | Servant API which provides access to wallet.
-- TODO: Should be composed depending on the resource - wallets, txs, ... http://haskell-servant.github.io/tutorial/0.4/server.html#nested-apis
type WalletApi =
     -- only works in development mode, gives 403 otherwise
     "api"
     :> "test"
     :> "reset"
     :> Post '[JSON] (Either WalletError ())
    :<|>
     -------------------------------------------------------------------------
     -- Wallet sets
     -------------------------------------------------------------------------
     "api"
     :> "walletSets"
     :> Capture "walletSetId" CWalletSetAddress
     :> Get '[JSON] (Either WalletError CWalletSet)
    :<|>
     "api"
     :> "walletSets"
     :> Get '[JSON] (Either WalletError [CWalletSet])
    :<|>
     "api"
     :> "walletSets"
     :> Capture "passphrase" CPassPhrase
     :> ReqBody '[JSON] CWalletSetInit
     :> Post '[JSON] (Either WalletError CWalletSet)
    :<|>
     "api"
     :> "walletSets"
     :> "restore"
     :> Capture "passphrase" CPassPhrase
     :> ReqBody '[JSON] CWalletSetInit
     :> Post '[JSON] (Either WalletError CWalletSet)
    :<|>
     "api"
     :> "walletSets"
     :> "keys"
     :> ReqBody '[JSON] Text
     :> Post '[JSON] (Either WalletError CWalletSet)
    :<|>
     -------------------------------------------------------------------------
     -- Wallets
     -------------------------------------------------------------------------
     "api"
     :> "wallets"
     :> Capture "walletId" CWalletAddress
     :> Get '[JSON] (Either WalletError CWallet)
    :<|>
     "api"
     :> "wallets"
     :> Capture "walletSetId" (Maybe CWalletSetAddress)
     :> Get '[JSON] (Either WalletError [CWallet])
    :<|>
     "api"
     :> "wallets"
     :> Capture "walletId" CWalletAddress
     :> ReqBody '[JSON] CWalletMeta
     :> Put '[JSON] (Either WalletError CWallet)
    :<|>
     "api"
     :> "wallets"
     :> Capture "passphrase" CPassPhrase
     :> ReqBody '[JSON] CWalletInit
     :> Post '[JSON] (Either WalletError CWallet)
    :<|>
     "api"
     :> "wallets"
     :> Capture "walletId" CWalletAddress
     :> Delete '[JSON] (Either WalletError ())
    :<|>
     -------------------------------------------------------------------------
     -- Accounts
     -------------------------------------------------------------------------
     "api"
     :> "wallets"
     :> Capture "passphrase" CPassPhrase
     :> ReqBody '[JSON] CWalletAddress
     :> Post '[JSON] (Either WalletError CAccount)
    :<|>
     ----------------------------------------------------------------------------
     -- Addresses
     ----------------------------------------------------------------------------
     "api"
     :> "addresses"
     :> Capture "address" Text
     :> "currencies"
     :> Capture "currency" CCurrency
     :> Get '[JSON] (Either WalletError Bool)
    :<|>
     ----------------------------------------------------------------------------
     -- Profile(s)
     ----------------------------------------------------------------------------
     -- TODO: A single profile? Should be possible in the future to have multiple profiles?
     "api"
     :> "profile"
     :> Get '[JSON] (Either WalletError CProfile)
    :<|>
     "api"
     :> "profile"
     :> ReqBody '[JSON] CProfile
     :> Post '[JSON] (Either WalletError CProfile)
    :<|>
     ----------------------------------------------------------------------------
     -- Transactons
     ----------------------------------------------------------------------------
    -- TODO: for now we only support one2one sending. We should extend this to support many2many
     "api"
     :> "txs"
     :> "payments"
     :> Capture "passphrase" CPassPhrase
     :> Capture "from" CAccountAddress
     :> Capture "to" CAccountAddress
     :> Capture "amount" Coin
     :> Post '[JSON] (Either WalletError CTx)
    :<|>
    -- TODO: for now we only support one2one sending. We should extend this to support many2many
     "api"
     :> "txs"
     :> "payments"
     :> Capture "passphrase" CPassPhrase
     :> Capture "from" CAccountAddress
     :> Capture "to" CAccountAddress
     :> Capture "amount" Coin
     :> Capture "currency" CCurrency
     :> Capture "title" Text
     :> Capture "description" Text
     :> Post '[JSON] (Either WalletError CTx)
    :<|>
      -- FIXME: Should capture the URL parameters in the payload.
      "api"
      :> "txs"
      :> "payments"
      :> Capture "address" CAccountAddress
      :> Capture "transaction" CTxId
      :> ReqBody '[JSON] CTxMeta
      :> Post '[JSON] (Either WalletError ())
    :<|>
     "api"
     :> "txs"
     :> "histories"
     :> Capture "walletId" CWalletAddress
     :> QueryParam "skip" Word
     :> QueryParam "limit" Word
     :> Get '[JSON] (Either WalletError ([CTx], Word))
    :<|>
     "api"
     :> "txs"
     :> "histories"
     :> Capture "walletId" CWalletAddress
     :> Capture "search" Text
     :> Capture "address" (Maybe CAccountAddress)
     :> QueryParam "skip" Word
     :> QueryParam "limit" Word
     :> Get '[JSON] (Either WalletError ([CTx], Word))
    :<|>
     ----------------------------------------------------------------------------
     -- Updates
     ----------------------------------------------------------------------------
     "api"
     :> "update"
     :> Get '[JSON] (Either WalletError CUpdateInfo)
    :<|>
     "api"
     :> "update"
     :> Post '[JSON] (Either WalletError ())
    :<|>
     ----------------------------------------------------------------------------
     -- Redemptions
     ----------------------------------------------------------------------------
     "api"
     :> "redemptions"
     :> "ada"
     :> ReqBody '[JSON] CWalletRedeem
     :> Post '[JSON] (Either WalletError CTx)
    :<|>
     ----------------------------------------------------------------------------
     -- Reporting
     ----------------------------------------------------------------------------
     "api"
     :> "reporting"
     :> "initialized"
     :> ReqBody '[JSON] CInitialized
     :> Post '[JSON] (Either WalletError ())
    :<|>
     ----------------------------------------------------------------------------
     -- Settings
     ----------------------------------------------------------------------------
     "api"
     :> "settings"
     :> "slots"
     :> "duration"
     :> Get '[JSON] (Either WalletError Word)
    :<|>
     "api"
     :> "settings"
     :> "version"
     :> Get '[JSON] (Either WalletError SoftwareVersion)
    :<|>
     "api"
     :> "settings"
     :> "sync"
     :> "progress"
     :> Get '[JSON] (Either WalletError SyncProgress)

-- | Helper Proxy.
walletApi :: Proxy WalletApi
walletApi = Proxy
