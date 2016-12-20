{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
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
import           Formatting                           (ords, sformat, stext, (%))
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
import           Pos.Slotting                         (getCurrentSlot)
import           Pos.Ssc.Class                        (SscConstraint)
import           Pos.Ssc.GodTossing                   (SscGodTossing, getOpening,
                                                       gtcParticipateSsc, isCommitmentIdx,
                                                       isOpeningIdx, isSharesIdx,
                                                       secretToSharedSeed)
import           Pos.Ssc.GodTossing.SecretStorage     (getSecret)
import qualified Pos.State                            as St
import           Pos.Txp.LocalData                    (TxLocalData, getLocalTxs,
                                                       getTxLocalData, setTxLocalData)
import           Pos.Types                            (EpochIndex (..), SharedSeed,
                                                       SlotId (siEpoch, siSlot),
                                                       SlotLeaders, headerHash)
import           Pos.Util                             (fromBinaryM)
import           Pos.Web.Api                          (BaseNodeApi, GodTossingApi,
                                                       GtNodeApi, baseNodeApi, gtNodeApi)
import           Pos.Web.Types                        (GodTossingStage (..))
import           Pos.WorkMode                         (TxLDImpl, WorkMode, runTxLDImpl)

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

type WebHandler ssc = TxLDImpl (ContextHolder ssc (St.DBHolder ssc TimedIO))

convertHandler
    :: forall ssc a.
       TxLocalData
    -> NodeContext ssc
    -> St.NodeState ssc
    -> WebHandler ssc a
    -> Handler a
convertHandler tld nc ns handler =
    liftIO (runTimedIO .
            St.runDBHolder ns .
            runContextHolder nc .
            runTxLDImpl $
            setTxLocalData tld >> handler)
    `Catch.catches`
    excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError

nat :: MyWorkMode ssc m => m (WebHandler ssc :~> Handler)
nat = do
    tld <- getTxLocalData
    nc <- getNodeContext
    ns <- St.getNodeState
    return $ Nat (convertHandler tld nc ns)

servantServerBase :: forall ssc m . MyWorkMode ssc m => m (Server (BaseNodeApi ssc))
servantServerBase = flip enter baseServantHandlers <$> (nat @ssc @m)

servantServerGT :: forall m . WorkMode SscGodTossing m => m (Server GtNodeApi)
servantServerGT = flip enter (baseServantHandlers :<|> gtServantHandlers) <$>
    (nat @SscGodTossing @m)

----------------------------------------------------------------------------
-- Base handlers
----------------------------------------------------------------------------

baseServantHandlers
    :: SscConstraint ssc
    => ServerT (BaseNodeApi ssc) (WebHandler ssc)
baseServantHandlers =
    getCurrentSlot :<|> getLeaders :<|> (ncPublicKey <$> getNodeContext) :<|>
    (headerHash <$> St.getHeadBlock) :<|> getLocalTxsNum

getLeaders :: SscConstraint ssc => Maybe EpochIndex -> WebHandler ssc SlotLeaders
getLeaders e = maybe (throwM err) pure =<< getLeadersDo e
  where
    epochStr = maybe "current" (sformat ords) e
    err =
        err404
        { errBody =
            encodeUtf8 $
            sformat ("Leaders are not know for "%stext%" epoch") epochStr
        }

getLeadersDo
    :: SscConstraint ssc
    => Maybe EpochIndex -> WebHandler ssc (Maybe SlotLeaders)
getLeadersDo Nothing  = St.getLeaders . siEpoch =<< getCurrentSlot
getLeadersDo (Just e) = St.getLeaders e

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
