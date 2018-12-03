{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

-- | Web server.

module Pos.Web.Server
       ( serveImpl
       , serveDocImpl
       , withRoute53HealthCheckApplication
       , serveWeb
       , servantServer
       , application
       , nodeServantHandlers
       ) where

import           Universum

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception.Safe as E
import           Control.Monad.Except (MonadError (throwError))
import qualified Control.Monad.Reader as Mtl
import qualified Data.ByteString.Char8 as BSC
import           Data.Default (def)
import           Data.Streaming.Network (bindPortTCP, bindRandomPortTCP)
import           Data.X509 (ExtKeyUsagePurpose (..), HashALG (..))
import           Data.X509.CertificateStore (readCertificateStore)
import           Data.X509.Validation (ValidationChecks (..),
                     ValidationHooks (..))
import qualified Data.X509.Validation as X509
import           Network.TLS (CertificateRejectReason (..),
                     CertificateUsage (..), ServerHooks (..))
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (Settings, defaultSettings, getHost,
                     runSettingsSocket, setHost, setPort)
import           Network.Wai.Handler.WarpTLS (TLSSettings (..), runTLSSocket,
                     tlsSettingsChain)
import           Servant.API ((:<|>) ((:<|>)))
import           Servant.Server (Handler, HasServer, ServantErr (errBody),
                     Server, ServerT, err503, hoistServer, serve)
import           UnliftIO (MonadUnliftIO)

import           Network.Socket (Socket, close)
import           Pos.Chain.Update (UpdateConfiguration)
import           Pos.Context (HasNodeContext (..), NodeContext)
import           Pos.DB (MonadDBRead)
import qualified Pos.DB as DB
import           Pos.DB.Txp (GenericTxpLocalData, MempoolExt,
                     getAllPotentiallyHugeUtxo, withTxpLocalData)
import qualified Pos.GState as GS
import           Pos.Infra.Reporting.Health.Types (HealthStatus (..))
import           Pos.Util.Util (HasLens, HasLens', lensOf)
import           Pos.Web.Mode (WebMode, WebModeContext (..))
import           Pos.WorkMode.Class (WorkMode)

import           Pos.Web.Api (HealthCheckApi, NodeApi, healthCheckApi, nodeApi)
import           Pos.Web.Types (CConfirmedProposalState (..), TlsParams (..))

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

type MyWorkMode ctx m =
    ( WorkMode ctx m
    , HasNodeContext ctx -- for ConvertHandler
    )

withRoute53HealthCheckApplication
    :: IO HealthStatus
    -> String
    -> Word16
    -> IO x
    -> IO x
withRoute53HealthCheckApplication mStatus host port act = Async.withAsync go (const act)
  where
    go = serveImpl (pure app) host port Nothing Nothing Nothing
    app = route53HealthCheckApplication mStatus

route53HealthCheckApplication :: IO HealthStatus -> Application
route53HealthCheckApplication mStatus =
    serve healthCheckApi (servantServerHealthCheck mStatus)

serveWeb :: MyWorkMode ctx m => Word16 -> Maybe TlsParams -> m ()
serveWeb port mTlsParams = serveImpl application "127.0.0.1" port mTlsParams Nothing Nothing

application :: MyWorkMode ctx m => m Application
application = do
    server <- servantServer
    return $ serve nodeApi server

serveTLS
    :: (MonadIO m)
    => (String -> Word16 -> TlsParams -> TLSSettings)
    -> m Application
    -> String
    -> Word16
    -- ^ if the port is 0, bind to a random port
    -> Maybe TlsParams
    -- ^ if isJust, use https, isNothing, use raw http
    -> Maybe Settings
    -> Maybe (Word16 -> IO ())
    -- ^ if isJust, call it with the port after binding
    -> m ()
serveTLS getTLSSettings app host port mWalletTLSParams mSettings mPortCallback = do
    app' <- app
    let
        acquire :: IO (Word16, Socket)
        acquire = liftIO $ if port == 0
            then do
                (port', socket) <- bindRandomPortTCP (getHost mySettings)
                pure (fromIntegral port', socket)
            else do
                socket <- bindPortTCP (fromIntegral port) (getHost mySettings)
                pure (port, socket)
        release :: (Word16, Socket) -> IO ()
        release (_, socket) = liftIO $ close socket
        action :: (Word16, Socket) -> IO ()
        action (port', socket) = do
            -- TODO: requires warp 3.2.17 setSocketCloseOnExec socket
            launchServer app' (port', socket)
    liftIO $ bracket acquire release action
  where
        launchServer :: Application -> (Word16, Socket) -> IO ()
        launchServer app'' (port', socket) = do
            fromMaybe (const (pure ())) mPortCallback port'
            maybe runSettingsSocket runTLSSocket mTlsConfig mySettings socket app''
        mySettings = setHost (fromString host) $
                     setPort (fromIntegral port) $
                     fromMaybe defaultSettings mSettings
        mTlsConfig = getTLSSettings host port <$> mWalletTLSParams

serveImpl
    :: (MonadIO m)
    => m Application
    -> String
    -> Word16
    -> Maybe TlsParams
    -> Maybe Settings
    -> Maybe (Word16 -> IO ())
    -> m ()
serveImpl =
    serveTLS tlsWithClientCheck

serveDocImpl
    :: (MonadIO m)
    => m Application
    -> String
    -> Word16
    -> Maybe TlsParams
    -> Maybe Settings
    -> Maybe (Word16 -> IO ())
    -> m ()
serveDocImpl =
    serveTLS tlsWebServerMode

tlsWebServerMode
    :: String -> Word16 -> TlsParams -> TLSSettings
tlsWebServerMode _ _ TlsParams{..} = tlsSettings
    { tlsWantClientCert = False }
  where
    tlsSettings =
      tlsSettingsChain tpCertPath [tpCaPath] tpKeyPath

tlsWithClientCheck
    :: String -> Word16 -> TlsParams -> TLSSettings
tlsWithClientCheck host port TlsParams{..} = tlsSettings
    { tlsWantClientCert = tpClientAuth
    , tlsServerHooks    = def
        { onClientCertificate = fmap certificateUsageFromValidations . validateCertificate }
    }
  where
    tlsSettings =
        tlsSettingsChain tpCertPath [tpCaPath] tpKeyPath

    serviceID =
        (host, BSC.pack (show port))

    certificateUsageFromValidations =
        maybe CertificateUsageAccept (CertificateUsageReject . CertificateRejectOther)

    -- By default, X509.Validation validates the certificate names against the host
    -- which is irrelevant when checking the client certificate (but relevant for
    -- the client when checking the server's certificate).
    validationHooks = def
        { hookValidateName = \_ _ -> [] }

    -- Here we add extra checks as the ones performed by default to enforce that
    -- the client certificate is actually _meant_ to be used for client auth.
    -- This should prevent server certificates to be used to authenticate
    -- against the server.
    validationChecks = def
        { checkStrictOrdering = True
        , checkLeafKeyPurpose = [KeyUsagePurpose_ClientAuth]
        }

    -- This solely verify that the provided certificate is valid and was signed by authority we
    -- recognize (tpCaPath)
    validateCertificate cert = do
        mstore <- readCertificateStore tpCaPath
        maybe
            (pure $ Just "Cannot init a store, unable to validate client certificates")
            (fmap fromX509FailedReasons . (\store -> X509.validate HashSHA256 validationHooks validationChecks store def serviceID cert))
            mstore

    fromX509FailedReasons reasons =
        case reasons of
            [] -> Nothing
            _  -> Just (show reasons)

----------------------------------------------------------------------------
-- Servant infrastructure
----------------------------------------------------------------------------

convertHandler
    :: forall ext a
    . UpdateConfiguration
    -> NodeContext
    -> DB.NodeDBs
    -> GenericTxpLocalData ext
    -> WebMode ext a
    -> Handler a
convertHandler uc nc nodeDBs txpData handler =
    liftIO
        (Mtl.runReaderT
             handler
             (WebModeContext nodeDBs txpData nc uc)) `E.catches`
    excHandlers
  where
    excHandlers = [E.Handler catchServant]
    catchServant = throwError

withNat
    :: forall ext ctx api m .
    (MyWorkMode ctx m, MempoolExt m ~ ext, HasServer api '[])
    => Proxy api -> ServerT api (WebMode ext) -> m (Server api)
withNat apiP handlers = do
    nc <- view nodeContext
    uc <- view (lensOf @UpdateConfiguration)
    nodeDBs <- DB.getNodeDBs
    txpLocalData <- withTxpLocalData return
    return $ hoistServer apiP (convertHandler uc nc nodeDBs txpLocalData) handlers

servantServer
    :: forall ctx m.
       MyWorkMode ctx m
    => m (Server NodeApi)
servantServer = withNat (Proxy @NodeApi) nodeServantHandlers

----------------------------------------------------------------------------
-- Node handlers
----------------------------------------------------------------------------

nodeServantHandlers
    ::
    ( MonadReader r m, MonadUnliftIO m, MonadDBRead m
    , HasLens UpdateConfiguration r UpdateConfiguration
    ) => ServerT NodeApi m
nodeServantHandlers =
    getAllPotentiallyHugeUtxo
    :<|>
    confirmedProposals

-- | Get info on all confirmed proposals
confirmedProposals
    :: (MonadDBRead m, MonadUnliftIO m, MonadReader r m, HasLens' r UpdateConfiguration)
    => m [CConfirmedProposalState]
confirmedProposals = do
    uc <- view (lensOf @UpdateConfiguration)
    proposals <- GS.getConfirmedProposals uc Nothing
    pure $ map (CConfirmedProposalState . show) proposals

----------------------------------------------------------------------------
-- HealthCheck handlers
----------------------------------------------------------------------------

servantServerHealthCheck :: IO HealthStatus -> Server HealthCheckApi
servantServerHealthCheck mStatus = do
    status <- liftIO mStatus
    case status of
      HSUnhealthy msg -> throwM $ err503 { errBody = encodeUtf8 msg }
      HSHealthy msg   -> return (show msg)
