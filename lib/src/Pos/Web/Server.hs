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
       , application
       ) where

import           Universum

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception.Safe as E
import           Control.Monad.Except (MonadError (throwError))
import qualified Control.Monad.Reader as Mtl
import qualified Data.ByteString.Char8 as BSC
import           Data.Default (Default, def)
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
                     Server, ServerT, err404, err503, hoistServer, serve)
import           UnliftIO (MonadUnliftIO)

import           Network.Socket (Socket, close)
import           Pos.Chain.Ssc (scParticipateSsc)
import           Pos.Chain.Txp (TxOut (..), toaOut)
import           Pos.Chain.Update (HasUpdateConfiguration)
import           Pos.Context (HasNodeContext (..), HasSscContext (..),
                     NodeContext, getOurPublicKey)
import           Pos.Core (EpochIndex (..), SlotLeaders)
import           Pos.DB (MonadDBRead)
import qualified Pos.DB as DB
import qualified Pos.DB.Lrc as LrcDB
import           Pos.DB.Txp (GenericTxpLocalData, MempoolExt,
                     getAllPotentiallyHugeUtxo, getLocalTxs, withTxpLocalData)
import qualified Pos.GState as GS
import           Pos.Infra.Reporting.Health.Types (HealthStatus (..))
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
    , Default (MempoolExt m)
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
    :: forall ext a.
       NodeContext
    -> DB.NodeDBs
    -> GenericTxpLocalData ext
    -> WebMode ext a
    -> Handler a
convertHandler nc nodeDBs txpData handler =
    liftIO
        (Mtl.runReaderT
             handler
             (WebModeContext nodeDBs txpData nc)) `E.catches`
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
    nodeDBs <- DB.getNodeDBs
    txpLocalData <- withTxpLocalData return
    return $ hoistServer apiP (convertHandler nc nodeDBs txpLocalData) handlers

servantServer
    :: forall ctx m.
       MyWorkMode ctx m
    => m (Server NodeApi)
servantServer = withNat (Proxy @NodeApi) nodeServantHandlers

----------------------------------------------------------------------------
-- Node handlers
----------------------------------------------------------------------------

nodeServantHandlers
    :: (HasUpdateConfiguration, Default ext)
    => ServerT NodeApi (WebMode ext)
nodeServantHandlers =
    getLeaders
    :<|>
    getUtxo
    :<|>
    getOurPublicKey
    :<|>
    GS.getTip
    :<|>
    getLocalTxsNum
    :<|>
    confirmedProposals
    :<|>
    toggleSscParticipation
    -- :<|> sscHasSecret
    -- :<|> getOurSecret
    -- :<|> getSscStage

getLeaders :: Maybe EpochIndex -> WebMode ext SlotLeaders
getLeaders maybeEpoch = do
    -- epoch <- maybe (siEpoch <$> getCurrentSlot) pure maybeEpoch
    epoch <- maybe (pure 0) pure maybeEpoch
    maybe (throwM err) pure =<< LrcDB.getLeadersForEpoch epoch
  where
    err = err404 { errBody = encodeUtf8 ("Leaders are not know for current epoch"::Text) }

getUtxo :: WebMode ext [TxOut]
getUtxo = map toaOut . toList <$> getAllPotentiallyHugeUtxo

getLocalTxsNum :: Default ext => WebMode ext Word
getLocalTxsNum = fromIntegral . length <$> withTxpLocalData getLocalTxs

-- | Get info on all confirmed proposals
confirmedProposals
    :: (HasUpdateConfiguration, MonadDBRead m, MonadUnliftIO m)
    => m [CConfirmedProposalState]
confirmedProposals = do
    proposals <- GS.getConfirmedProposals Nothing
    pure $ map (CConfirmedProposalState . show) proposals

toggleSscParticipation :: Bool -> WebMode ext ()
toggleSscParticipation enable =
    view sscContext >>=
    atomically . flip writeTVar enable . scParticipateSsc

-- sscHasSecret :: SscWebHandler Bool
-- sscHasSecret = isJust <$> getSecret

-- getOurSecret :: SscWebHandler SharedSeed
-- getOurSecret = maybe (throw err) (pure . convertSscSecret) =<< getSecret
--   where
--     err = err404 { errBody = "I don't have secret" }
--     doPanic = panic "our secret is malformed"
--     convertSscSecret =
--         secretToSharedSeed .
--         fromMaybe doPanic . fromBinaryM . getOpening . view _2

-- getSscStage :: SscWebHandler SscStage
-- getSscStage = do
--     getSscStageImpl . siSlot <$> getCurrentSlot
--   where
--     getSscStageImpl idx
--         | isCommitmentIdx idx = CommitmentStage
--         | isOpeningIdx idx = OpeningStage
--         | isSharesIdx idx = SharesStage
--         | otherwise = OrdinaryStage

----------------------------------------------------------------------------
-- HealthCheck handlers
----------------------------------------------------------------------------

servantServerHealthCheck :: IO HealthStatus -> Server HealthCheckApi
servantServerHealthCheck mStatus = do
    status <- liftIO mStatus
    case status of
      HSUnhealthy msg -> throwM $ err503 { errBody = encodeUtf8 msg }
      HSHealthy msg   -> return (show msg)
