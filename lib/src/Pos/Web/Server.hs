{-# LANGUAGE TypeOperators #-}

-- | Web server.

module Pos.Web.Server
       ( MyWorkMode
       , WebMode
       , serveImplNoTLS
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
import           Pos.Ssc.Class                   (SscConstraint)
import           Pos.Ssc.GodTossing              (SscGodTossing, gtcParticipateSsc)
import           Pos.Txp                         (TxOut (..), toaOut)
import           Pos.Txp.MemState                (GenericTxpLocalData, askTxpMem,
                                                  getLocalTxs)
import           Pos.Web.Mode                    (WebMode, WebModeContext (..))
import           Pos.WorkMode                    (OQ)
import           Pos.WorkMode.Class              (TxpExtra_TMP, WorkMode)

import           Pos.Web.Api                     (BaseNodeApi, GodTossingApi, GtNodeApi,
                                                  HealthCheckApi, baseNodeApi, gtNodeApi,
                                                  healthCheckApi)
import           Pos.Web.Types                   (TlsParams (..))

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

-- [CSL-152]: I want SscConstraint to be part of WorkMode.
type MyWorkMode ssc ctx m =
    ( WorkMode ssc ctx m
    , SscConstraint ssc
    , HasNodeContext ssc ctx -- for ConvertHandler
    )

serveWebBase :: MyWorkMode ssc ctx m => Word16 -> Maybe TlsParams -> m ()
serveWebBase = serveImpl applicationBase "127.0.0.1"

applicationBase :: MyWorkMode ssc ctx m => m Application
applicationBase = do
    server <- servantServerBase
    return $ serve baseNodeApi server

route53HealthCheckApplication :: MyWorkMode ssc ctx m => Topology t -> OQ m -> m Application
route53HealthCheckApplication topology oq = do
    server <- servantServerHealthCheck topology oq
    return $ serve healthCheckApi server

serveWebGT :: MyWorkMode SscGodTossing ctx m => Word16 -> Maybe TlsParams -> m ()
serveWebGT = serveImpl applicationGT "127.0.0.1"

applicationGT :: MyWorkMode SscGodTossing ctx m => m Application
applicationGT = do
    server <- servantServerGT
    return $ serve gtNodeApi server

serveImplNoTLS :: (HasConfiguration, MonadIO m) => m Application -> String -> Word16 -> m ()
serveImplNoTLS application host port =
    liftIO . runSettings mySettings =<< application
  where
    mySettings = setHost (fromString host) $
                 setPort (fromIntegral port) defaultSettings

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
    :: forall ssc a.
       NodeContext ssc
    -> DB.NodeDBs
    -> GenericTxpLocalData TxpExtra_TMP
    -> WebMode ssc a
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

nat :: forall ssc ctx m . MyWorkMode ssc ctx m => m (WebMode ssc :~> Handler)
nat = do
    nc <- view nodeContext
    nodeDBs <- DB.getNodeDBs
    txpLocalData <- askTxpMem
    return $ NT (convertHandler nc nodeDBs txpLocalData)

servantServerBase :: forall ssc ctx m . MyWorkMode ssc ctx m => m (Server (BaseNodeApi ssc))
servantServerBase = flip enter baseServantHandlers <$> (nat @ssc @ctx @m)

servantServerHealthCheck :: forall ssc ctx t m . MyWorkMode ssc ctx m => Topology t -> OQ m -> m (Server HealthCheckApi)
servantServerHealthCheck topology oq = flip enter (healthCheckServantHandlers topology oq) <$> (nat @ssc @ctx @m)

servantServerGT :: forall ctx m . MyWorkMode SscGodTossing ctx m => m (Server GtNodeApi)
servantServerGT = flip enter (baseServantHandlers :<|> gtServantHandlers) <$>
    (nat @SscGodTossing @ctx @m)

----------------------------------------------------------------------------
-- Base handlers
----------------------------------------------------------------------------

baseServantHandlers :: HasConfiguration => ServerT (BaseNodeApi ssc) (WebMode ssc)
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

getLeaders :: HasConfiguration => Maybe EpochIndex -> WebMode ssc SlotLeaders
getLeaders maybeEpoch = do
    -- epoch <- maybe (siEpoch <$> getCurrentSlot) pure maybeEpoch
    epoch <- maybe (pure 0) pure maybeEpoch
    maybe (throwM err) pure =<< LrcDB.getLeadersForEpoch epoch
  where
    err = err404 { errBody = encodeUtf8 ("Leaders are not know for current epoch"::Text) }

getUtxo :: HasConfiguration => WebMode ssc [TxOut]
getUtxo = map toaOut . toList <$> GS.getAllPotentiallyHugeUtxo

getLocalTxsNum :: WebMode ssc Word
getLocalTxsNum = fromIntegral . length <$> getLocalTxs

----------------------------------------------------------------------------
-- HealthCheck handlers
----------------------------------------------------------------------------

healthCheckServantHandlers :: Topology t -> OQ m -> ServerT HealthCheckApi (WebMode ssc)
healthCheckServantHandlers topology oq =
    getRoute53HealthCheck topology oq

getRoute53HealthCheck :: Topology t -> OQ m -> ServerT HealthCheckApi (WebMode ssc)
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

type GtWebMode = WebMode SscGodTossing

gtServantHandlers :: ServerT GodTossingApi GtWebMode
gtServantHandlers =
    toggleGtParticipation {- :<|> gtHasSecret :<|> getOurSecret :<|> getGtStage -}

toggleGtParticipation :: Bool -> GtWebMode ()
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
