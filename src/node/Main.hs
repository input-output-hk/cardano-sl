{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main
  ( main
  ) where

import           Control.Monad.Trans        (MonadTrans)
import           Data.Maybe                 (fromJust)
import qualified Data.Set                   as S (fromList)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Data.Time.Units            (toMicroseconds)
import qualified Ether
import           Formatting                 (sformat, shown, (%))
import           Mockable                   (Production, currentTime)
import           Network.Transport.Abstract (Transport, hoistTransport)
import qualified Network.Transport.TCP      as TCP (TCPAddr (..), TCPAddrInfo (..))
import           Node                       (hoistSendActions)
import           Pos.Util.TimeWarp          (addressToNodeId)
import           Serokell.Util              (sec)
import           System.Wlog                (logError, logInfo)
import           Universum

import qualified Data.ByteString.Char8      as BS8 (unpack)

import           Pos.Binary                 ()
import qualified Pos.CLI                    as CLI
import           Pos.Communication          (ActionSpec (..), NodeId, OutSpecs,
                                             WorkerSpec, worker, wrapActionSpec)
import           Pos.Constants              (isDevelopment)
import           Pos.Context                (MonadNodeContext)
import           Pos.Core.Types             (Timestamp (..))
import           Pos.DHT.Real               (KademliaDHTInstance (..),
                                             foreverRejoinNetwork)
import           Pos.DHT.Workers            (dhtWorkers)
import           Pos.Discovery              (askDHTInstance, getPeers, runDiscoveryConstT,
                                             runDiscoveryKademliaT)
import           Pos.Launcher               (NodeParams (..), bracketResources,
                                             bracketResourcesKademlia, runNode,
                                             runNodeProduction, runNodeStatic,
                                             runNodeStats)
import           Pos.Shutdown               (triggerShutdown)
import           Pos.Ssc.Class              (SscConstraint, SscParams)
import           Pos.Ssc.GodTossing         (SscGodTossing)
import           Pos.Ssc.NistBeacon         (SscNistBeacon)
import           Pos.Ssc.SscAlgo            (SscAlgo (..))
import           Pos.Statistics             (getNoStatsT, getStatsMap, runStatsT')
import           Pos.Update.Context         (ucUpdateSemaphore)
import           Pos.Util                   (inAssertMode)
import           Pos.Util.UserSecret        (usVss)
import           Pos.Util.Util              (powerLift)
import           Pos.Wallet                 (WalletSscType)
import           Pos.WorkMode               (ProductionMode, RawRealMode, RawRealModeK,
                                             StaticMode, StatsMode)
#ifdef WITH_WEB
import           Pos.Web                    (serveWebGT)
import           Pos.WorkMode               (WorkMode)
#ifdef WITH_WALLET
import           Pos.Wallet.Web             (WalletProductionMode, WalletStaticMode,
                                             WalletStatsMode, WalletWebHandler,
                                             bracketWalletWS, bracketWalletWebDB,
                                             runWProductionMode, runWStaticMode,
                                             runWStatsMode, walletServeWebFull,
                                             walletServeWebFullS, walletServerOuts)
#endif
#endif

import           NodeOptions                (Args (..), getNodeOptions)
import           Params                     (getBaseParams, getKademliaParams,
                                             getNodeParams, getPeersFromArgs, gtSscParams)

action
    :: Either KademliaDHTInstance (Set NodeId)
    -> Args
    -> (forall ssc . Transport (RawRealMode ssc))
    -> Production ()
action peerHolder args@Args {..} transport = do
    systemStart <- getNodeSystemStart $ CLI.sysStart commonArgs
    logInfo $ sformat ("System start time is " % shown) systemStart
    t <- currentTime
    logInfo $ sformat ("Current time is " % shown) (Timestamp t)
    currentParams <- getNodeParams args systemStart
    putText $ "Running using " <> show (CLI.sscAlgo commonArgs)
    putText $ "If stats is on: " <> show enableStats
    putText $ "Static peers is on: " <> show staticPeers

    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
    let gtParams = gtSscParams args vssSK
    let wDhtWorkers :: WorkMode ssc m => KademliaDHTInstance -> ([WorkerSpec m], OutSpecs)
        wDhtWorkers = first (map $ wrapActionSpec $ "worker" <> "dht") . dhtWorkers

#ifdef WITH_WEB
    when enableWallet $ do
        let currentPluginsGT :: (MonadNodeContext SscGodTossing m, WorkMode SscGodTossing m) => [m ()]
            currentPluginsGT = pluginsGT args
        bracketWalletWebDB walletDbPath walletRebuildDb $ \db ->
            bracketWalletWS $ \conn -> do
                let allPlugins :: (MonadNodeContext SscGodTossing m, WorkMode SscGodTossing m)
                               => KademliaDHTInstance
                               -> ([WorkerSpec m], OutSpecs)
                    allPlugins ki = mconcat [wDhtWorkers ki, convPlugins currentPluginsGT]

                case (peerHolder, enableStats, CLI.sscAlgo commonArgs) of
                    (Right peers, _, GodTossingAlgo) ->
                        runWStaticMode db conn
                            transportW peers
                            currentParams gtParams
                            (runNode @SscGodTossing $ (convPlugins currentPluginsGT) <> walletStatic args)
                    (Left kad, True, GodTossingAlgo) ->
                        runWStatsMode db conn
                            transportW kad
                            currentParams gtParams
                            (runNode @SscGodTossing (allPlugins kad <> walletStats args))
                    (Left kad, False, GodTossingAlgo) ->
                        runWProductionMode db conn
                            transportW kad
                            currentParams gtParams
                            (runNode @SscGodTossing (allPlugins kad <> walletProd args))
                    (_, _, NistBeaconAlgo) ->
                        logError "Wallet does not support NIST beacon!"
#endif
    if not enableWallet then do
        let sscParams :: Either (SscParams SscNistBeacon) (SscParams SscGodTossing)
            sscParams = bool (Left ()) (Right gtParams) (CLI.sscAlgo commonArgs == GodTossingAlgo)

        case (peerHolder, enableStats) of
            (Right peers, _) -> do
                let runner :: forall ssc . SscConstraint ssc => SscParams ssc -> Production ()
                    runner =
                        runNodeStatic @ssc
                            transportR
                            peers
                            utwStatic
                            currentParams
                either (runner @SscNistBeacon) (runner @SscGodTossing) sscParams
            (Left kad, True) -> do
                let runner :: forall ssc . SscConstraint ssc => SscParams ssc -> Production ()
                    runner =
                        runNodeStats @ssc
                            transportR
                            kad
                            (mconcat [wDhtWorkers kad, utwStats])
                            currentParams
                either (runner @SscNistBeacon) (runner @SscGodTossing) sscParams
            (Left kad, False) -> do
                let runner :: forall ssc . SscConstraint ssc => SscParams ssc -> Production ()
                    runner =
                        runNodeProduction @ssc
                            transportR
                            kad
                            (mconcat [wDhtWorkers kad, utwProd])
                            currentParams
                either (runner @SscNistBeacon) (runner @SscGodTossing) sscParams
    else
        logError $ "You try to run wallet, but code wasn't compiled with wallet flag"
  where
    convPlugins = (,mempty) . map (\act -> ActionSpec $ \__vI __sA -> act)

    transportR ::
        forall ssc t0 t1 .
        ( Each '[MonadTrans] [t0, t1]
        , Each '[Monad] [ t1 (RawRealMode ssc)
                        , t0 (t1 (RawRealMode ssc))
                        ]
        )
        => Transport (t0 $ t1 (RawRealMode ssc))
    transportR = hoistTransport (lift . lift) transport

    transportW ::
        forall ssc t0 t1 t2 t3 .
        ( Each '[MonadTrans] [t0, t1, t2, t3]
        , Each '[Monad] [ t3 (RawRealMode ssc)
                        , t2 (t3 (RawRealMode ssc))
                        , t1 (t2 (t3 (RawRealMode ssc)))
                        , t0 (t1 (t2 (t3 (RawRealMode ssc))))]
        )
        => Transport (t0 $ t1 $ t2 $ t3 (RawRealMode ssc))
    transportW = hoistTransport (lift . lift . lift . lift) transport

#ifdef WITH_WEB
pluginsGT ::
    ( WorkMode SscGodTossing m
    , MonadNodeContext SscGodTossing m
    ) => Args -> [m ()]
pluginsGT Args {..}
    | enableWeb = [serveWebGT webPort]
    | otherwise = []
#endif

utwProd :: SscConstraint ssc => ([WorkerSpec (ProductionMode ssc)], OutSpecs)
utwProd = first (map liftPlugin) updateTriggerWorker
  where
    liftPlugin (ActionSpec p) = ActionSpec $ \vI sa -> do
        ki <- askDHTInstance
        lift . lift . p vI $ hoistSendActions (runDiscoveryKademliaT ki . getNoStatsT) (lift . lift) sa

utwStats :: SscConstraint ssc => ([WorkerSpec (StatsMode ssc)], OutSpecs)
utwStats = first (map liftPlugin) updateTriggerWorker
  where
    liftPlugin (ActionSpec p) = ActionSpec $ \vI sa -> do
        s <- getStatsMap
        ki <- askDHTInstance
        lift . lift . p vI $ hoistSendActions (runDiscoveryKademliaT ki . runStatsT' s) (lift . lift) sa

utwStatic :: SscConstraint ssc => ([WorkerSpec (StaticMode ssc)], OutSpecs)
utwStatic = first (map liftPlugin) updateTriggerWorker
  where
    liftPlugin (ActionSpec p) = ActionSpec $ \vI sa -> do
        peers <- getPeers
        lift . lift . p vI $ hoistSendActions (runDiscoveryConstT peers . getNoStatsT) (lift . lift) sa

updateTriggerWorker
    :: SscConstraint ssc
    => ([WorkerSpec (RawRealMode ssc)], OutSpecs)
updateTriggerWorker = first pure $ worker mempty $ \_ -> do
    logInfo "Update trigger worker is locked"
    void $ takeMVar =<< Ether.asks' ucUpdateSemaphore
    triggerShutdown

----------------------------------------------------------------------------
-- Wallet stuff
----------------------------------------------------------------------------

walletProd
    :: SscConstraint WalletSscType
    => Args
    -> ([WorkerSpec WalletProductionMode], OutSpecs)
walletProd args = first (map liftPlugin) (walletServe args)
  where
    liftPlugin (ActionSpec p) = ActionSpec $ \vI sa ->
        lift . p vI $ hoistSendActions getNoStatsT lift sa

walletStats
    :: SscConstraint WalletSscType
    => Args
    -> ([WorkerSpec WalletStatsMode], OutSpecs)
walletStats args = first (map liftPlugin) (walletServe args)
  where
    liftPlugin (ActionSpec p) = ActionSpec $ \vI sa -> do
        sm <- getStatsMap
        lift . p vI $ hoistSendActions (runStatsT' sm) lift sa

walletStatic
    :: SscConstraint WalletSscType
    => Args
    -> ([WorkerSpec WalletStaticMode], OutSpecs)
walletStatic Args{..} = first (map liftPlugin) walletServeStatic
  where
    liftPlugin (ActionSpec p) = ActionSpec $ \vI sa ->
        lift . p vI $ hoistSendActions getNoStatsT lift sa
    walletServeStatic = first pure $ worker walletServerOuts $ \sendActions ->
        walletServeWebFullS
            sendActions
            walletDebug
            walletPort

walletServe
    :: SscConstraint WalletSscType
    => Args
    -> ([WorkerSpec (WalletWebHandler (RawRealModeK WalletSscType))], OutSpecs)
walletServe Args {..} = first pure $ worker walletServerOuts $ \sendActions ->
    walletServeWebFull
        sendActions
        walletDebug
        walletPort

printFlags :: IO ()
printFlags = do
    if isDevelopment
        then putText "[Attention] We are in DEV mode"
        else putText "[Attention] We are in PRODUCTION mode"
#ifdef WITH_WEB
    putText "[Attention] Web-mode is on"
#endif
#ifdef WITH_WALLET
    putText "[Attention] Wallet-mode is on"
#endif
    inAssertMode $ putText "Asserts are ON"


getNodeSystemStart :: MonadIO m => Timestamp -> m Timestamp
getNodeSystemStart cliOrConfigSystemStart
  | cliOrConfigSystemStart >= 1400000000 =
    -- UNIX time 1400000000 is Tue, 13 May 2014 16:53:20 GMT.
    -- It was chosen arbitrarily as some date far enough in the past.
    -- See CSL-983 for more information.
    pure cliOrConfigSystemStart
  | otherwise = do
    let frameLength = timestampToSeconds cliOrConfigSystemStart
    currentPOSIXTime <- liftIO $ round <$> getPOSIXTime
    -- The whole timeline is split into frames, with the first frame starting
    -- at UNIX epoch start. We're looking for a time `t` which would be in the
    -- middle of the same frame as the current UNIX time.
    let currentFrame = currentPOSIXTime `div` frameLength
        t = currentFrame * frameLength + (frameLength `div` 2)
    pure $ Timestamp $ sec $ fromIntegral t
  where
    timestampToSeconds :: Timestamp -> Integer
    timestampToSeconds = (`div` 1000000) . toMicroseconds . getTimestamp

main :: IO ()
main = do
    printFlags
    args <- getNodeOptions
    let baseParams = getBaseParams "node" args
    if staticPeers args then do
        allPeers <- S.fromList . map addressToNodeId <$> getPeersFromArgs args
        bracketResources baseParams TCP.Unaddressable $ \transport -> do
            let transport' = hoistTransport
                    (powerLift :: forall ssc t . Production t -> RawRealMode ssc t)
                    transport
            action (Right allPeers) args transport'
    else do
        let (bindHost, bindPort) = bindAddress args
        let (externalHost, externalPort) = externalAddress args
        let tcpAddr = TCP.Addressable $
                TCP.TCPAddrInfo (BS8.unpack bindHost) (show $ bindPort)
                                (const (BS8.unpack externalHost, show $ externalPort))
        kademliaParams <- liftIO $ getKademliaParams args
        bracketResourcesKademlia baseParams tcpAddr kademliaParams $ \kademliaInstance transport ->
            let transport' = hoistTransport
                    (powerLift :: forall ssc t . Production t -> RawRealMode ssc t)
                    transport
            in  foreverRejoinNetwork kademliaInstance (action (Left kademliaInstance) args transport')
