{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Web server.

module Pos.Web.Server
       ( MyWorkMode
       , WebHandler
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
import           Data.Tagged                          (Tagged (..))
import qualified Ether
import           Mockable                             (Production (runProduction))
import           Network.Wai                          (Application)
import           Network.Wai.Handler.Warp             (defaultSettings, runSettings,
                                                       setHost, setPort)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant.API                          ((:<|>) ((:<|>)), FromHttpApiData)
import           Servant.Server                       (Handler, ServantErr (errBody),
                                                       Server, ServerT, err404, serve)
import           Servant.Utils.Enter                  ((:~>) (NT), enter)

import           Pos.Aeson.Types                      ()
import           Pos.Context                          (MonadNodeContext, NodeContext,
                                                       NodeContextTag, SscContextTag,
                                                       npPublicKey)
import qualified Pos.DB                               as DB
import qualified Pos.DB.GState                        as GS
import           Pos.DB.Redirect                      (DBPureRedirect, runDBPureRedirect)
import qualified Pos.Lrc.DB                           as LrcDB
import           Pos.Ssc.Class                        (SscConstraint)
import           Pos.Ssc.GodTossing                   (SscGodTossing, gtcParticipateSsc)
import           Pos.Txp                              (TxOut (..), toaOut)
import           Pos.Txp.MemState                     (GenericTxpLocalData, TxpHolderTag,
                                                       askTxpMem, getLocalTxs)
import           Pos.Types                            (EpochIndex (..), SlotLeaders)
import           Pos.WorkMode.Class                   (TxpExtra_TMP, WorkMode)

import           Pos.Web.Api                          (BaseNodeApi, GodTossingApi,
                                                       GtNodeApi, baseNodeApi, gtNodeApi)
-- import           Pos.Web.Types                        (GodTossingStage (..))

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

-- [CSL-152]: I want SscConstraint to be part of WorkMode.
type MyWorkMode ssc m =
    ( WorkMode ssc m
    , SscConstraint ssc
    , MonadNodeContext ssc m -- for ConvertHandler
    )

serveWebBase :: MyWorkMode ssc m => Word16 -> m ()
serveWebBase = serveImpl applicationBase "127.0.0.1"

applicationBase :: MyWorkMode ssc m => m Application
applicationBase = do
    server <- servantServerBase
    return $ serve baseNodeApi server

serveWebGT :: MyWorkMode SscGodTossing m => Word16 -> m ()
serveWebGT = serveImpl applicationGT "127.0.0.1"

applicationGT :: MyWorkMode SscGodTossing m => m Application
applicationGT = do
    server <- servantServerGT
    return $ serve gtNodeApi server

-- [CSL-217]: do not hardcode logStdoutDev.
serveImpl :: MonadIO m => m Application -> String -> Word16 -> m ()
serveImpl application host port =
    liftIO . runSettings mySettings . logStdoutDev =<< application
  where
    mySettings = setHost (fromString host) $
                 setPort (fromIntegral port) defaultSettings

----------------------------------------------------------------------------
-- Servant infrastructure
----------------------------------------------------------------------------

type WebHandler ssc =
    DBPureRedirect $
    Ether.ReadersT
        ( Tagged DB.NodeDBs DB.NodeDBs
        , Tagged TxpHolderTag (GenericTxpLocalData TxpExtra_TMP)
        ) (
    Ether.ReadersT (NodeContext ssc) Production
    )

convertHandler
    :: forall ssc a.
       NodeContext ssc
    -> DB.NodeDBs
    -> GenericTxpLocalData TxpExtra_TMP
    -> WebHandler ssc a
    -> Handler a
convertHandler nc nodeDBs wrap handler =
    liftIO (runProduction .
            flip Ether.runReadersT nc .
            flip Ether.runReadersT
              ( Tagged @DB.NodeDBs nodeDBs
              , Tagged @TxpHolderTag wrap
              ) .
            runDBPureRedirect $
            handler)
    `Catch.catches`
    excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError

nat :: forall ssc m . MyWorkMode ssc m => m (WebHandler ssc :~> Handler)
nat = do
    nc <- Ether.ask @NodeContextTag
    nodeDBs <- DB.getNodeDBs
    txpLocalData <- askTxpMem
    return $ NT (convertHandler nc nodeDBs txpLocalData)

servantServerBase :: forall ssc m . MyWorkMode ssc m => m (Server (BaseNodeApi ssc))
servantServerBase = flip enter baseServantHandlers <$> (nat @ssc @m)

servantServerGT :: forall m . MyWorkMode SscGodTossing m => m (Server GtNodeApi)
servantServerGT = flip enter (baseServantHandlers :<|> gtServantHandlers) <$>
    (nat @SscGodTossing @m)

----------------------------------------------------------------------------
-- Base handlers
----------------------------------------------------------------------------

baseServantHandlers :: ServerT (BaseNodeApi ssc) (WebHandler ssc)
baseServantHandlers =
    getLeaders
    :<|>
    getUtxo
    :<|>
    (Ether.asks' npPublicKey)
    :<|>
    GS.getTip
    :<|>
    getLocalTxsNum

getLeaders :: Maybe EpochIndex -> WebHandler ssc SlotLeaders
getLeaders maybeEpoch = do
    -- epoch <- maybe (siEpoch <$> getCurrentSlot) pure maybeEpoch
    epoch <- maybe (pure 0) pure maybeEpoch
    maybe (throwM err) pure =<< LrcDB.getLeaders epoch
  where
    err = err404 { errBody = encodeUtf8 ("Leaders are not know for current epoch"::Text) }

getUtxo :: WebHandler ssc [TxOut]
getUtxo = map toaOut . toList <$> GS.getAllPotentiallyHugeUtxo

getLocalTxsNum :: WebHandler ssc Word
getLocalTxsNum = fromIntegral . length <$> getLocalTxs

----------------------------------------------------------------------------
-- GodTossing handlers
----------------------------------------------------------------------------

type GtWebHandler = WebHandler SscGodTossing

gtServantHandlers :: ServerT GodTossingApi GtWebHandler
gtServantHandlers =
    toggleGtParticipation {- :<|> gtHasSecret :<|> getOurSecret :<|> getGtStage -}

toggleGtParticipation :: Bool -> GtWebHandler ()
toggleGtParticipation enable =
    Ether.ask @SscContextTag >>=
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
