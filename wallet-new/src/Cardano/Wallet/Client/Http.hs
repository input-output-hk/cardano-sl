{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Client.Http
    ( module Cardano.Wallet.Client.Http
      -- * Abstract Client export
    , module Cardano.Wallet.Client
    -- * Servant Client Export
    , module Servant.Client
    -- * Helper to load X509 certificates and private key
    , credentialLoadX509
    , newManager
    , Manager
    ) where

import           Universum

import           Control.Lens (_Left)
import           Data.Aeson (decode)
import           Data.ByteString (ByteString)
import           Data.Default.Class (Default (..))
import           Data.X509 (CertificateChain, SignedCertificate)
import           Data.X509.CertificateStore (makeCertificateStore)
import           Network.Connection (TLSSettings (..))
import           Network.HTTP.Client (Manager, ManagerSettings,
                     defaultManagerSettings, newManager)
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.TLS (ClientHooks (..), ClientParams (..),
                     Credentials (..), HostName, PrivKey, Shared (..),
                     Supported (..), credentialLoadX509, noSessionManager)
import           Network.TLS.Extra.Cipher (ciphersuite_default)
import           Servant ((:<|>) (..), (:>))
import           Servant.Client (BaseUrl (..), ClientEnv (..), ClientM,
                     Scheme (..), ServantError (..), client, runClientM)

import qualified Cardano.Wallet.API.V1 as V1
import           Cardano.Wallet.Client


type Port = ByteString


mkHttpManagerSettings :: ManagerSettings
mkHttpManagerSettings =
    defaultManagerSettings


mkHttpsManagerSettings
    :: (HostName, Port)              -- ^ Target server hostname & port
    -> [SignedCertificate]           -- ^ CA certificate chain
    -> (CertificateChain, PrivKey)   -- ^ (Client certificate, Client key)
    -> ManagerSettings
mkHttpsManagerSettings serverId caChain credentials =
    mkManagerSettings tlsSettings sockSettings
  where
    sockSettings = Nothing
    tlsSettings  = TLSSettings clientParams
    clientParams = ClientParams
        { clientUseMaxFragmentLength    = Nothing
        , clientServerIdentification    = serverId
        , clientUseServerNameIndication = True
        , clientWantSessionResume       = Nothing
        , clientShared                  = clientShared
        , clientHooks                   = clientHooks
        , clientSupported               = clientSupported
        , clientDebug                   = def
        }
    clientShared = Shared
        { sharedCredentials     = Credentials [credentials]
        , sharedCAStore         = makeCertificateStore caChain
        , sharedSessionManager  = noSessionManager
        , sharedValidationCache = def
        }
    clientHooks = def
        { onCertificateRequest = const . return . Just $ credentials
        }
    clientSupported = def
        { supportedCiphers = ciphersuite_default
        }


-- | Given a 'BaseUrl' and an @http-client@ 'Manager', this returns
-- a 'WalletClient' that operates in 'IO'.
mkHttpClient
    :: BaseUrl
    -> Manager
    -> WalletClient IO
mkHttpClient baseUrl manager = WalletClient
    { getAddressIndexPaginated
        = \x -> run . getAddressIndexR x
    , postAddress
        = run . postAddressR
    , getAddress
        = run . getAddressR
    -- wallets endpoints
    , postWallet
        = run . postWalletR
    , getWalletIndexFilterSorts
        = \mp mpp filters sorts -> run $
            getWalletIndexFilterSortsR mp mpp filters sorts
    , updateWalletPassword
        = \x -> run . updateWalletPasswordR x
    , deleteWallet
        = unNoContent . run . deleteWalletR
    , getWallet
        = run . getWalletR
    , updateWallet
        = \x -> run . updateWalletR x
    , postCheckExternalWallet
        = run . postCheckExternalWalletR
    , postExternalWallet
        = run . postExternalWalletR
    , deleteExternalWallet
        = unNoContent . run . deleteExternalWalletR
    -- account endpoints
    , deleteAccount
        = \x -> unNoContent . run . deleteAccountR x
    , getAccount
        = \x -> run . getAccountR x
    , getAccountIndexPaged
        = \x y -> run . getAccountIndexPagedR x y
    , postAccount
        = \w -> run . postAccountR w
    , updateAccount
        = \x y -> run . updateAccountR x y
    , redeemAda
        = run ... redeemAdaR
    , postAddressPath
        = \x -> run . postAddressPathR x
    , postStoreAddress
        = \x addr -> unNoContent . run . postStoreAddressR x addr
    -- transactions endpoints
    , postTransaction
        = run . postTransactionR
    , getTransactionIndexFilterSorts
        = \walletId mAccountIndex mAddress mPage mpp filters ->
             run . getTransactionIndexFilterSortsR walletId mAccountIndex mAddress mPage mpp filters
    , getTransactionFee
        = run . getTransactionFeeR
    , postUnsignedTransaction
        = run . postUnsignedTransactionR
    , postSignedTransaction
        = run . postSignedTransactionR
    -- settings
    , getNodeSettings
        = run getNodeSettingsR
    -- info
    , getNodeInfo
        = run getNodeInfoR
    }

  where

    -- Must give the type. GHC will not infer it to be polymorphic in 'a'.
    run :: forall a . ClientM a -> IO (Either ClientError a)
    run = fmap (over _Left parseJsendError) . (`runClientM` clientEnv)

    unNoContent = map void
    cookieJar = Nothing
    clientEnv = ClientEnv manager baseUrl cookieJar
    parseJsendError servantErr =
        case servantErr of
            FailureResponse resp ->
                case decode (responseBody resp) of
                    Just err -> ClientWalletError err
                    Nothing  -> ClientHttpError servantErr
            _ -> ClientHttpError servantErr
    getAddressIndexR
        :<|> postAddressR
        :<|> getAddressR
        = addressesAPI

    postWalletR
        :<|> getWalletIndexFilterSortsR
        :<|> updateWalletPasswordR
        :<|> deleteWalletR
        :<|> getWalletR
        :<|> updateWalletR
        :<|> postCheckExternalWalletR
        :<|> postExternalWalletR
        :<|> deleteExternalWalletR
        = walletsAPI

    deleteAccountR
        :<|> getAccountR
        :<|> getAccountIndexPagedR
        :<|> postAccountR
        :<|> updateAccountR
        :<|> redeemAdaR
        :<|> postAddressPathR
        :<|> postStoreAddressR
        = accountsAPI

    postTransactionR
        :<|> getTransactionIndexFilterSortsR
        :<|> getTransactionFeeR
        :<|> postUnsignedTransactionR
        :<|> postSignedTransactionR
        = transactionsAPI

    addressesAPI
        :<|> walletsAPI
        :<|> accountsAPI
        :<|> transactionsAPI
        :<|> getNodeSettingsR
        :<|> getNodeInfoR
        = client (Proxy @("api" :> "v1" :> V1.API))
