{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
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

import           Control.Concurrent.STM.TVar          (writeTVar)
import           Control.Lens                         (view, _3)
import qualified Control.Monad.Catch                  as Catch
import           Control.Monad.Except                 (MonadError (throwError))
import           Control.TimeWarp.Timed               (TimedIO, runTimedIO)
import           Network.Wai                          (Application)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant.API                          ((:<|>) ((:<|>)), FromHttpApiData)
import           Servant.Server                       (Handler, ServantErr (errBody),
                                                       Server, ServerT, err404, serve)
import           Servant.Utils.Enter                  ((:~>) (Nat), enter)
import           Universum

import           Pos.Aeson.Types                      ()
import           Pos.Context                          (ContextHolder, NodeContext,
                                                       getNodeContext, ncPublicKey,
                                                       ncSscContext, runContextHolder)
import qualified Pos.DB                               as DB
import qualified Pos.DB.GState                        as GS
import qualified Pos.DB.Lrc                           as LrcDB
import           Pos.Slotting                         (getCurrentSlot)
import           Pos.Ssc.Class                        (SscConstraint)
import           Pos.Ssc.GodTossing                   (SscGodTossing, getOpening,
                                                       gtcParticipateSsc, isCommitmentIdx,
                                                       isOpeningIdx, isSharesIdx,
                                                       secretToSharedSeed)
import           Pos.Ssc.GodTossing.SecretStorage     (getSecret)
import           Pos.Txp.Class                        (getLocalTxs, getTxpLDWrap)
import           Pos.Txp.Holder                       (TxpLDHolder, TxpLDWrap,
                                                       runTxpLDHolderReader)
import           Pos.Types                            (EpochIndex (..), SharedSeed,
                                                       SlotId (..), SlotLeaders, siSlot)
import           Pos.Util                             (fromBinaryM)
import           Pos.Web.Api                          (BaseNodeApi, GodTossingApi,
                                                       GtNodeApi, baseNodeApi, gtNodeApi)
import           Pos.Web.Types                        (GodTossingStage (..))
import           Pos.WorkMode                         (WorkMode)

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

-- [CSL-152]: I want SscConstraint to be part of WorkMode.
type MyWorkMode ssc m = (WorkMode ssc m, SscConstraint ssc)

serveWebBase :: MyWorkMode ssc m => Word16 -> m ()
serveWebBase = serveImpl applicationBase

applicationBase :: MyWorkMode ssc m => m Application
applicationBase = do
    server <- servantServerBase
    return $ serve baseNodeApi server

serveWebGT :: MyWorkMode SscGodTossing m => Word16 -> m ()
serveWebGT = serveImpl applicationGT

applicationGT :: MyWorkMode SscGodTossing m => m Application
applicationGT = do
    server <- servantServerGT
    return $ serve gtNodeApi server

-- [CSL-217]: do not hardcode logStdoutDev.
serveImpl :: MonadIO m => m Application -> Word16 -> m ()
serveImpl application port =
    liftIO . run (fromIntegral port) . logStdoutDev =<< application

----------------------------------------------------------------------------
-- Servant infrastructure
----------------------------------------------------------------------------

type WebHandler ssc = ContextHolder ssc (TxpLDHolder ssc (DB.DBHolder ssc TimedIO))
-- type WebHandler ssc = TxLDImpl (ContextHolder ssc (St.DBHolder ssc TimedIO))

convertHandler
    :: forall ssc a.
       -- TxLocalData
       NodeContext ssc
    -> DB.NodeDBs ssc
    -> TxpLDWrap ssc
    -> WebHandler ssc a
    -> Handler a
convertHandler nc nodeDBs wrap handler =
    liftIO (runTimedIO .
            DB.runDBHolder nodeDBs .
            runTxpLDHolderReader wrap .
            runContextHolder nc $
            handler)
            -- runTxLDImpl $
            -- setTxLocalData tld >> handler)
    `Catch.catches`
    excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError

nat :: forall ssc m . (MyWorkMode ssc m)
    => m (WebHandler ssc :~> Handler)
nat = do
    nc <- getNodeContext
    nodeDBs <- DB.getNodeDBs
    -- Is this legal at all???
    (txpldwrap :: TxpLDWrap ssc) <- getTxpLDWrap
    return $ Nat (convertHandler nc nodeDBs txpldwrap)

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
    getCurrentSlot :<|> const getLeaders :<|> (ncPublicKey <$> getNodeContext) :<|>
    GS.getTip :<|> getLocalTxsNum

getLeaders :: WebHandler ssc SlotLeaders
getLeaders = do
    SlotId{..} <- getCurrentSlot
    maybe (throwM err) pure =<< LrcDB.getLeaders siEpoch
  where
    err = err404 { errBody = encodeUtf8 ("Leaders are not know for current epoch"::Text) }

getLocalTxsNum :: WebHandler ssc Word
getLocalTxsNum = fromIntegral . length <$> getLocalTxs

----------------------------------------------------------------------------
-- GodTossing handlers
----------------------------------------------------------------------------

type GtWebHandler = WebHandler SscGodTossing

gtServantHandlers :: ServerT GodTossingApi GtWebHandler
gtServantHandlers =
    toggleGtParticipation :<|> gtHasSecret :<|> getOurSecret :<|> getGtStage

toggleGtParticipation :: Bool -> GtWebHandler ()
toggleGtParticipation enable =
    getNodeContext >>=
    atomically . flip writeTVar enable . gtcParticipateSsc . ncSscContext

gtHasSecret :: GtWebHandler Bool
gtHasSecret = isJust <$> getSecret

getOurSecret :: GtWebHandler SharedSeed
getOurSecret = maybe (throwM err) (pure . convertGtSecret) =<< getSecret
  where
    err = err404 { errBody = "I don't have secret" }
    doPanic = panic "our secret is malformed"
    convertGtSecret =
        secretToSharedSeed .
        fromMaybe doPanic . fromBinaryM . getOpening . view _3

getGtStage :: GtWebHandler GodTossingStage
getGtStage = do
    getGtStageImpl . siSlot <$> getCurrentSlot
  where
    getGtStageImpl idx
        | isCommitmentIdx idx = CommitmentStage
        | isOpeningIdx idx = OpeningStage
        | isSharesIdx idx = SharesStage
        | otherwise = OrdinaryStage

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

deriving instance FromHttpApiData EpochIndex
