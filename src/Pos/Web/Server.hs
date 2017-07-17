{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Web server.

module Pos.Web.Server
       ( MyWorkMode
       , WebMode
       , serveImpl
       , nat
       , serveWebBase
       , applicationBase
       , serveWebGT
       , applicationGT
       ) where

import           Universum

import qualified Control.Monad.Catch                  as Catch
import           Control.Monad.Except                 (MonadError (throwError))
import qualified Control.Monad.Reader                 as Mtl
import           Mockable                             (Production (runProduction))
import           Network.Wai                          (Application)
import           Network.Wai.Handler.Warp             (defaultSettings, setHost, setPort)
import           Network.Wai.Handler.WarpTLS          (runTLS, tlsSettingsChain)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant.API                          ((:<|>) ((:<|>)), FromHttpApiData)
import           Servant.Server                       (Handler, ServantErr (errBody),
                                                       Server, ServerT, err404, serve)
import           Servant.Utils.Enter                  ((:~>) (NT), enter)

import           Pos.Aeson.Types                      ()
import           Pos.Context                          (HasNodeContext (..),
                                                       HasSscContext (..), NodeContext,
                                                       getOurPublicKey)
import           Pos.Core                             (EpochIndex (..), SlotLeaders)
import qualified Pos.DB                               as DB
import qualified Pos.DB.GState                        as GS
import qualified Pos.Lrc.DB                           as LrcDB
import           Pos.Ssc.Class                        (SscConstraint)
import           Pos.Ssc.GodTossing                   (SscGodTossing, gtcParticipateSsc)
import           Pos.Txp                              (TxOut (..), toaOut)
import           Pos.Txp.MemState                     (GenericTxpLocalData, askTxpMem,
                                                       getLocalTxs, ignoreTxpMetrics)
import           Pos.Web.Mode                         (WebMode, WebModeContext (..))
import           Pos.WorkMode.Class                   (TxpExtra_TMP, WorkMode)

import           Pos.Web.Api                          (BaseNodeApi, GodTossingApi,
                                                       GtNodeApi, baseNodeApi, gtNodeApi)
-- import           Pos.Web.Types                        (GodTossingStage (..))

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

-- [CSL-152]: I want SscConstraint to be part of WorkMode.
type MyWorkMode ssc ctx m =
    ( WorkMode ssc ctx m
    , SscConstraint ssc
    , HasNodeContext ssc ctx -- for ConvertHandler
    )

serveWebBase :: MyWorkMode ssc ctx m => Word16 -> FilePath -> FilePath -> FilePath -> m ()
serveWebBase = serveImpl applicationBase "127.0.0.1"

applicationBase :: MyWorkMode ssc ctx m => m Application
applicationBase = do
    server <- servantServerBase
    return $ serve baseNodeApi server

serveWebGT :: MyWorkMode SscGodTossing ctx m => Word16 -> FilePath -> FilePath -> FilePath -> m ()
serveWebGT = serveImpl applicationGT "127.0.0.1"

applicationGT :: MyWorkMode SscGodTossing ctx m => m Application
applicationGT = do
    server <- servantServerGT
    return $ serve gtNodeApi server

-- [CSL-217]: do not hardcode logStdoutDev.
serveImpl :: MonadIO m => m Application -> String -> Word16 -> FilePath -> FilePath -> FilePath -> m ()
serveImpl application host port walletTLSCert walletTLSKey walletTLSca =
    liftIO . runTLS tlsConfig mySettings . logStdoutDev =<< application
  where
    mySettings = setHost (fromString host) $
                 setPort (fromIntegral port) defaultSettings
    tlsConfig = tlsSettingsChain walletTLSCert [walletTLSca] walletTLSKey

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
             (WebModeContext nodeDBs (txpData, ignoreTxpMetrics) nc)) `Catch.catches`
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

servantServerGT :: forall ctx m . MyWorkMode SscGodTossing ctx m => m (Server GtNodeApi)
servantServerGT = flip enter (baseServantHandlers :<|> gtServantHandlers) <$>
    (nat @SscGodTossing @ctx @m)

----------------------------------------------------------------------------
-- Base handlers
----------------------------------------------------------------------------

baseServantHandlers :: ServerT (BaseNodeApi ssc) (WebMode ssc)
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

getLeaders :: Maybe EpochIndex -> WebMode ssc SlotLeaders
getLeaders maybeEpoch = do
    -- epoch <- maybe (siEpoch <$> getCurrentSlot) pure maybeEpoch
    epoch <- maybe (pure 0) pure maybeEpoch
    maybe (throwM err) pure =<< LrcDB.getLeaders epoch
  where
    err = err404 { errBody = encodeUtf8 ("Leaders are not know for current epoch"::Text) }

getUtxo :: WebMode ssc [TxOut]
getUtxo = map toaOut . toList <$> GS.getAllPotentiallyHugeUtxo

getLocalTxsNum :: WebMode ssc Word
getLocalTxsNum = fromIntegral . length <$> getLocalTxs

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
