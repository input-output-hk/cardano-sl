{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | Web server.

module Pos.Web.Server
       ( MyWorkMode
       , WebMode
       , serveImpl
       , nat
       , serveWebBase
       , applicationBase
       , route53HealthCheckApplication
       , serveWebGT
       , applicationGT
       ) where

import           Universum

import qualified Control.Monad.Catch             as Catch
import           Control.Monad.Except            (MonadError (throwError))
import qualified Control.Monad.Reader            as Mtl
import           Data.Default                    (Default)
import           Mockable                        (Production (runProduction))
import           Network.Wai                     (Application)
import           Network.Wai.Handler.Warp        (defaultSettings, runSettings, setHost,
                                                  setPort)
import           Network.Wai.Handler.WarpTLS     (TLSSettings, runTLS, tlsSettingsChain)
import           Servant.API                     ((:<|>) ((:<|>)), FromHttpApiData)
import           Servant.Server                  (Handler, ServantErr (errBody), Server,
                                                  ServerT, err404, err503, serve)
import           Servant.Utils.Enter             ((:~>) (NT), enter)

import qualified Network.Broadcast.OutboundQueue as OQ
import           Pos.Aeson.Types                 ()
import           Pos.Context                     (HasNodeContext (..), HasSscContext (..),
                                                  NodeContext, getOurPublicKey)
import           Pos.Core                        (EpochIndex (..), SlotLeaders)
import           Pos.Core.Configuration          (HasConfiguration)
import qualified Pos.DB                          as DB
import qualified Pos.GState                      as GS
import qualified Pos.Lrc.DB                      as LrcDB
import           Pos.Network.Types               (Bucket (BucketSubscriptionListener),
                                                  Topology, topologyMaxBucketSize)
import           Pos.Ssc.GodTossing              (gtcParticipateSsc)
import           Pos.Txp                         (TxOut (..), toaOut)
import           Pos.Txp.MemState                (GenericTxpLocalData, MempoolExt,
                                                  askTxpMem, getLocalTxs)
import           Pos.Web.Mode                    (WebMode, WebModeContext (..))
import           Pos.WorkMode                    (OQ)
import           Pos.WorkMode.Class              (WorkMode)

import           Pos.Web.Api                     (BaseNodeApi, GodTossingApi, GtNodeApi,
                                                  HealthCheckApi, baseNodeApi, gtNodeApi,
                                                  healthCheckApi)
import           Pos.Web.Types                   (TlsParams (..))

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

type MyWorkMode ctx m =
    ( WorkMode ctx m
    , HasNodeContext ctx -- for ConvertHandler
    , Default (MempoolExt m)
    )

serveWebBase :: MyWorkMode ctx m => Word16 -> Maybe TlsParams -> m ()
serveWebBase = serveImpl applicationBase "127.0.0.1"

applicationBase :: MyWorkMode ctx m => m Application
applicationBase = do
    server <- servantServerBase
    return $ serve baseNodeApi server

route53HealthCheckApplication :: MyWorkMode ctx m => Topology t -> OQ m -> m Application
route53HealthCheckApplication topology oq = do
    server <- servantServerHealthCheck topology oq
    return $ serve healthCheckApi server

serveWebGT :: MyWorkMode ctx m => Word16 -> Maybe TlsParams -> m ()
serveWebGT = serveImpl applicationGT "127.0.0.1"

applicationGT :: MyWorkMode ctx m => m Application
applicationGT = do
    server <- servantServerGT
    return $ serve gtNodeApi server

serveImpl
    :: (HasConfiguration, MonadIO m)
    => m Application -> String -> Word16 -> Maybe TlsParams -> m ()
serveImpl application host port walletTLSParams =
    liftIO . maybe runSettings runTLS mTlsConfig mySettings
        =<< application
  where
    mySettings = setHost (fromString host) $
                 setPort (fromIntegral port) defaultSettings
    mTlsConfig = tlsParamsToWai <$> walletTLSParams

tlsParamsToWai :: TlsParams -> TLSSettings
tlsParamsToWai TlsParams{..} = tlsSettingsChain tpCertPath [tpCaPath] tpKeyPath

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
        (runProduction $
         Mtl.runReaderT
             handler
             (WebModeContext nodeDBs txpData nc)) `Catch.catches`
    excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError

nat
    :: forall ext ctx m .
    ( MyWorkMode ctx m, MempoolExt m ~ ext)
    => m (WebMode ext :~> Handler)
nat = do
    nc <- view nodeContext
    nodeDBs <- DB.getNodeDBs
    txpLocalData <- askTxpMem
    return $ NT (convertHandler nc nodeDBs txpLocalData)

servantServerBase :: forall ctx m . MyWorkMode ctx m => m (Server BaseNodeApi)
servantServerBase = flip enter baseServantHandlers <$> (nat @(MempoolExt m) @ctx @m)

servantServerHealthCheck :: forall ctx t m . MyWorkMode ctx m => Topology t -> OQ m -> m (Server HealthCheckApi)
servantServerHealthCheck topology oq =
    flip enter (healthCheckServantHandlers topology oq) <$> (nat @(MempoolExt m) @ctx @m)

servantServerGT :: forall ctx m . MyWorkMode ctx m => m (Server GtNodeApi)
servantServerGT = flip enter (baseServantHandlers :<|> gtServantHandlers) <$>
    (nat @(MempoolExt m) @ctx @m)

----------------------------------------------------------------------------
-- Base handlers
----------------------------------------------------------------------------

baseServantHandlers :: (HasConfiguration, Default ext) => ServerT BaseNodeApi (WebMode ext)
baseServantHandlers =
    getLeaders
    :<|>
    getUtxo
    :<|>
    getOurPublicKey
    :<|>
    GS.getTip
    :<|>
    getLocalTxsNum

getLeaders :: HasConfiguration => Maybe EpochIndex -> WebMode ext SlotLeaders
getLeaders maybeEpoch = do
    -- epoch <- maybe (siEpoch <$> getCurrentSlot) pure maybeEpoch
    epoch <- maybe (pure 0) pure maybeEpoch
    maybe (throwM err) pure =<< LrcDB.getLeaders epoch
  where
    err = err404 { errBody = encodeUtf8 ("Leaders are not know for current epoch"::Text) }

getUtxo :: HasConfiguration => WebMode ext [TxOut]
getUtxo = map toaOut . toList <$> GS.getAllPotentiallyHugeUtxo

getLocalTxsNum :: Default ext => WebMode ext Word
getLocalTxsNum = fromIntegral . length <$> getLocalTxs

----------------------------------------------------------------------------
-- HealthCheck handlers
----------------------------------------------------------------------------

healthCheckServantHandlers :: Topology t -> OQ m -> ServerT HealthCheckApi (WebMode ext)
healthCheckServantHandlers topology oq =
    getRoute53HealthCheck topology oq

getRoute53HealthCheck :: Topology t -> OQ m -> ServerT HealthCheckApi (WebMode ext)
getRoute53HealthCheck (topologyMaxBucketSize -> getSize) oq = do
    let maxCapacityTxt = case getSize BucketSubscriptionListener of
                             OQ.BucketSizeUnlimited -> "unlimited"
                             (OQ.BucketSizeMax x)   -> fromString (show x)
    -- If the node doesn't have any more subscription slots available,
    -- mark the node as "unhealthy" by returning a 503 "Service Unavailable".
    spareCapacity <- OQ.bucketSpareCapacity oq BucketSubscriptionListener
    case spareCapacity of
        OQ.UnlimitedCapacity          ->
            return maxCapacityTxt -- yields "unlimited" as it means the `BucketMaxSize` was unlimited.
        OQ.SpareCapacity sc | sc == 0 -> throwM $ err503 { errBody = encodeUtf8 ("0/" <> maxCapacityTxt) }
        OQ.SpareCapacity sc           -> return $ show sc <> "/" <> maxCapacityTxt -- yields 200/OK

----------------------------------------------------------------------------
-- GodTossing handlers
----------------------------------------------------------------------------

type GtWebMode ext = WebMode ext

gtServantHandlers :: ServerT GodTossingApi (GtWebMode ext)
gtServantHandlers =
    toggleGtParticipation {- :<|> gtHasSecret :<|> getOurSecret :<|> getGtStage -}

toggleGtParticipation :: Bool -> GtWebMode ext ()
toggleGtParticipation enable =
    view sscContext >>=
    atomically . flip writeTVar enable . gtcParticipateSsc

-- gtHasSecret :: GtWebHandler Bool
-- gtHasSecret = isJust <$> getSecret

-- getOurSecret :: GtWebHandler SharedSeed
-- getOurSecret = maybe (throw err) (pure . convertGtSecret) =<< getSecret
--   where
--     err = err404 { errBody = "I don't have secret" }
--     doPanic = panic "our secret is malformed"
--     convertGtSecret =
--         secretToSharedSeed .
--         fromMaybe doPanic . fromBinaryM . getOpening . view _2

-- getGtStage :: GtWebHandler GodTossingStage
-- getGtStage = do
--     getGtStageImpl . siSlot <$> getCurrentSlot
--   where
--     getGtStageImpl idx
--         | isCommitmentIdx idx = CommitmentStage
--         | isOpeningIdx idx = OpeningStage
--         | isSharesIdx idx = SharesStage
--         | otherwise = OrdinaryStage

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

deriving instance FromHttpApiData EpochIndex
