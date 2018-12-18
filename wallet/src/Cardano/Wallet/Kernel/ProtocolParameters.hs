{-# LANGUAGE LambdaCase #-}
module Cardano.Wallet.Kernel.ProtocolParameters where

import           Universum

import qualified Data.ByteString.Char8 as B8
import           Servant.Client (BaseUrl (..), Scheme (..))

import           Cardano.Node.Client (NodeClient (..), NodeHttpClient,
                     mkHttpClient)
import           Cardano.Node.Manager (credentialLoadX509)
import           Cardano.Wallet.Server.CLI
import           Network.HTTP.Client (Manager, newManager)
--import           Cardano.Node.API
import           Cardano.Node.Manager (mkHttpsManagerSettings, readSignedObject)
--import qualified Pos.Node.API as P
import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Util.Wlog (logInfo)
import           Pos.Web.Types

import           Pos.Core.Slotting (SlotId (..))

import qualified Pos.Node.API as API

import qualified Pos.Core as Core
import           Pos.Node.API (SecurityParameter)
import           Serokell.Data.Memory.Units (Byte)

--data Cache k v = Cache (TVar (Maybe v)) (v -> k)
--
--cached :: Cache k a -> IO a -> IO a
--cached (Cache ref keyfun) a = do
--    x <- readTVarIO ref
--    case x of
--        Just v  -> return v
--        Nothing -> do
--                       v' <- a
--                       atomically $ writeTVar ref (Just v')
--                       return v'
--

data ProtocolParameterAdaptor = ProtocolParameterAdaptor
    {
      nodeClient           :: NodeHttpClient

      -- | Get slot ID of current tip
      --
      -- Tests must pass in an explicit value here.
    , getTipSlotId         :: IO Core.SlotId

      -- | Get maximum transaction size
    , getMaxTxSize         :: IO Byte

      -- | Get fee policy
    , getFeePolicy         :: IO Core.TxFeePolicy

      -- | Get the security parameter (@k@)
    , getSecurityParameter :: IO SecurityParameter

      -- | Get number of slots per epoch
      --
      -- This can be used as an input to 'flattenSlotIdExplicit'.
      --
      -- NOTE: If this constant ever changes, then we'd have to return something
      -- more detailed here ("slot count was X between epoch A and B, and Y
      -- thereafter"). However, the same change will have to be made to
      -- 'flattenSlotIdExplicit' in core as well as, probably, a ton of other
      -- places.
    , getSlotCount         :: IO Core.SlotCount

    -- | Get the @Genesis.Config@
    , getCoreConfig        :: IO Genesis.Config

--      -- | Get the start of a slot
--      --
--      -- When looking up data for past of the current epochs, the value should
--      -- be known.
--    , getSlotStart :: SlotId -> IO (Either UnknownEpoch Timestamp)
--
--      -- | Get last known slot duration.
--    , getNextEpochSlotDuration :: IO Millisecond
--
--      -- | Get the "sync progress". This term is desperately overloaded but
--      -- in this context we need something very simple: a tuple containing the
--      -- "global blockchain height" and the "node blockchain height". The
--      -- former is the maximum between the biggest height we observed from an
--      -- unsolicited block we received and the current local tip:
--      --
--      -- global_height = max (last_known_header, node_local_tip)
--      --
--      -- The latter is simply the node local tip, i.e. "how far we went into
--      -- chasing the global blockchain height during syncing".
--    , getNodeSyncProgress :: LockContext -> m (Maybe BlockCount, BlockCount)
--
        -- | Version of application (code running)
      -- , curSoftwareVersion :: m SoftwareVersion
--
--      -- | Git revision
--    , compileInfo :: m CompileTimeInfo
--
--      -- | Ask the NTP client for the status
--    , getNtpDrift :: V1.ForceNtpCheck -> m V1.TimeInfo
--
        -- | Get the current timestamp
        --
        -- Tests can mock transaction creation time with this function.
      --, getCreationTimestamp :: m Timestamp
    }

newProtocolParameterAdaptor :: NodeHttpClient -> ProtocolParameterAdaptor
newProtocolParameterAdaptor client = ProtocolParameterAdaptor
    { nodeClient           = client
    , getTipSlotId         = f $ API.setSlotId <$> getNodeSettings client
    , getMaxTxSize         = f $ API.setMaxTxSize <$> getNodeSettings client
    , getFeePolicy         = f $ API.setFeePolicy <$> getNodeSettings client
    , getSecurityParameter = error "TODO"--f $ API.securityParameter <$> getProtocolParameters client
    , getSlotCount         = f $ API.setSlotCount <$> getNodeSettings client
    , getCoreConfig        = error "TODO" --f $ API.coreConfig <$> getProtocolParameters client
    --, getCreationTimeStamp = f $ API.creationTimeStamp <$> getProtocolParameters client
    --, curSoftwareVersion   = f $ API.curSoftwareVersion <$> getProtocolParameters client
    }
      where
        -- TODO(@anviking #87): proper error handling
        f :: MonadIO m => Show e => Show a => ExceptT e m a -> m a
        f e = do
            x <- runExceptT e
            case x of
                Right a   -> liftIO $ logInfo (show a) >> return a
                Left  err -> error $ "ProtocolParameters:44" <> (show err)

setupClient :: NewWalletBackendParams -> IO (NodeHttpClient, Manager)
setupClient (NewWalletBackendParams params) = do
    let (serverHost', serverPort') = ("localhost", 8080 :: Int)
    let (serverHost, serverPort) = (B8.unpack serverHost', fromIntegral serverPort')
    let serverId = (serverHost, B8.pack $ show serverPort)


    let tlsParams = maybe (error "TODO") id $ walletTLSParams params
    logInfo $ show tlsParams
    let tlsPrivKeyPath    = "scripts/tls-files/client.pem"
    let tlsClientCertPath = "scripts/tls-files/client.crt"

    let tlsCACertPath = "scripts/tls-files/ca.crt"

    logInfo . show $ tpCaPath tlsParams

    logInfo $ "Localhost: " <> (show serverHost)
    logInfo $ "Priv key path: " <> (show tlsPrivKeyPath)

    caChain <- readSignedObject tlsCACertPath

    clientCredentials <- credentialLoadX509 tlsClientCertPath tlsPrivKeyPath >>= \case
        Right   a -> return a
        Left  err -> fail $ "Error decoding X509 certificates: " <> err
    manager <- newManager $ mkHttpsManagerSettings serverId caChain clientCredentials

    let
        baseUrl = BaseUrl Https serverHost serverPort mempty
        walletClient = mkHttpClient baseUrl manager

    return (walletClient, manager)
