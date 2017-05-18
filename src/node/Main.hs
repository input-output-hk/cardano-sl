{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Data.Maybe                 (fromJust)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Data.Time.Units            (toMicroseconds)
import qualified Ether
import           Formatting                 (sformat, shown, (%))
import           Mockable                   (Production, currentTime)
import           Network.Transport.Abstract (Transport, hoistTransport)
import qualified Network.Transport.TCP      as TCP (TCPAddr (..), TCPAddrInfo (..))
import           Node                       (hoistSendActions)
import           Serokell.Util              (sec)
import           System.Wlog                (logError, logInfo)
import           Universum

import qualified Data.ByteString.Char8      as BS8 (unpack)

import           Pos.Binary                 ()
import qualified Pos.CLI                    as CLI
import           Pos.Communication          (ActionSpec (..), OutSpecs, WorkerSpec,
                                             worker, wrapActionSpec)
import           Pos.Constants              (isDevelopment)
import           Pos.Context                (MonadNodeContext)
import           Pos.Core.Types             (Timestamp (..))
import           Pos.DHT.Real               (KademliaDHTInstance (..),
                                             foreverRejoinNetwork)
import           Pos.DHT.Workers            (dhtWorkers)
import           Pos.Launcher               (NodeParams (..), bracketResourcesKademlia,
                                             runNode, runNodeProduction, runNodeStats)
import           Pos.Shutdown               (triggerShutdown)
import           Pos.Ssc.Class              (SscConstraint)
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
                                             StatsMode)
#ifdef WITH_WEB
import           Pos.Web                    (serveWebBase, serveWebGT)
import           Pos.WorkMode               (WorkMode)
#ifdef WITH_WALLET
import           Pos.Wallet.Web             (WalletProductionMode, WalletStatsMode,
                                             WalletWebHandler, bracketWalletWS,
                                             bracketWalletWebDB, liftWMode,
                                             runWProductionMode, runWStatsMode,
                                             walletServeWebFull, walletServerOuts)
#endif
#endif

import           NodeOptions                (Args (..), getNodeOptions)
import           Params                     (getBaseParams, getKademliaParams,
                                             getNodeParams, gtSscParams)

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

action
    :: KademliaDHTInstance
    -> Args
    -> (forall ssc . Transport (RawRealMode ssc))
    -> Production ()
action kademliaInst args@Args {..} transport = do
    systemStart <- getNodeSystemStart $ CLI.sysStart commonArgs
    logInfo $ sformat ("System start time is " % shown) systemStart
    t <- currentTime
    logInfo $ sformat ("Current time is " % shown) (Timestamp t)
    currentParams <- getNodeParams args systemStart
    putText $ "Running using " <> show (CLI.sscAlgo commonArgs)
    putText $ "If stats is on: " <> show enableStats

    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
        gtParams = gtSscParams args vssSK
#ifdef WITH_WEB
    when enableWallet $ do
        let currentPluginsGT
                :: (MonadNodeContext SscGodTossing m, WorkMode SscGodTossing m)
                => [m ()]
            currentPluginsGT = pluginsGT args
        bracketWalletWebDB walletDbPath walletRebuildDb $ \db ->
            bracketWalletWS $ \conn -> do
                case (enableStats, CLI.sscAlgo commonArgs) of
                    (True, GodTossingAlgo) -> do
                        let liftedTransport =
                                hoistTransport (lift . liftWMode) transport
                            wDhtWorkers =
                                first
                                    (map $ wrapActionSpec $ "worker" <> "dht")
                                    (dhtWorkers kademliaInst)
                        let allPlugins :: ([WorkerSpec WalletStatsMode], OutSpecs)
                            allPlugins =
                                mconcat
                                    [ wDhtWorkers
                                    , convPlugins currentPluginsGT
                                    , walletStats args
                                    ]
                        runWStatsMode
                            db
                            conn
                            (CLI.peerId commonArgs)
                            liftedTransport
                            kademliaInst
                            currentParams
                            gtParams
                            (runNode @SscGodTossing allPlugins)
                    (False, GodTossingAlgo) -> do
                        let liftedTransport =
                                hoistTransport (lift . liftWMode) transport
                            wDhtWorkers =
                                first
                                    (map $ wrapActionSpec $ "worker" <> "dht")
                                    (dhtWorkers kademliaInst)
                        let allPlugins :: ([WorkerSpec WalletProductionMode], OutSpecs)
                            allPlugins =
                                mconcat
                                    [ wDhtWorkers
                                    , convPlugins currentPluginsGT
                                    , walletProd args
                                    ]
                        runWProductionMode
                            db
                            conn
                            (CLI.peerId commonArgs)
                            liftedTransport
                            kademliaInst
                            currentParams
                            gtParams
                            (runNode @SscGodTossing allPlugins)
                    (_, NistBeaconAlgo) ->
                        logError "Wallet does not support NIST beacon!"
#endif
    if not enableWallet then do
        let currentPlugins :: [a]
            currentPlugins = []
            currentPluginsGT :: [a]
            currentPluginsGT = []
        let wDhtWorkersStats :: ([WorkerSpec (StatsMode ssc')], OutSpecs)
            wDhtWorkersStats =
                first (map $ wrapActionSpec $ "worker" <> "dht") (dhtWorkers kademliaInst)
        let wDhtWorkersProd :: ([WorkerSpec (ProductionMode ssc')], OutSpecs)
            wDhtWorkersProd =
                first (map $ wrapActionSpec $ "worker" <> "dht") (dhtWorkers kademliaInst)
        case (enableStats, CLI.sscAlgo commonArgs) of
            (True, GodTossingAlgo) -> do
                let allPlugins :: ([WorkerSpec (StatsMode SscGodTossing)], OutSpecs)
                    allPlugins = mconcat [ wDhtWorkersStats
                                         , convPlugins currentPluginsGT
                                         , utwStats ]
                runNodeStats @SscGodTossing
                    (CLI.peerId commonArgs)
                    (hoistTransport (lift . lift) transport)
                    kademliaInst
                    allPlugins
                    currentParams gtParams
            (True, NistBeaconAlgo) -> do
                let allPlugins :: ([WorkerSpec (StatsMode SscNistBeacon)], OutSpecs)
                    allPlugins = mconcat [ wDhtWorkersStats
                                         , convPlugins currentPlugins
                                         , utwStats ]
                runNodeStats @SscNistBeacon
                    (CLI.peerId commonArgs)
                    (hoistTransport (lift . lift) transport)
                    kademliaInst
                    allPlugins
                    currentParams ()
            (False, GodTossingAlgo) -> do
                let allPlugins :: ([WorkerSpec (ProductionMode SscGodTossing)], OutSpecs)
                    allPlugins = mconcat [ wDhtWorkersProd
                                         , convPlugins currentPluginsGT
                                         , utwProd ]
                runNodeProduction @SscGodTossing
                    (CLI.peerId commonArgs)
                    (hoistTransport (lift . lift) transport)
                    kademliaInst
                    allPlugins
                    currentParams gtParams
            (False, NistBeaconAlgo) -> do
                let allPlugins :: ([WorkerSpec (ProductionMode SscNistBeacon)], OutSpecs)
                    allPlugins = mconcat [ wDhtWorkersProd
                                         , convPlugins currentPlugins
                                         , utwProd ]
                runNodeProduction @SscNistBeacon
                    (CLI.peerId commonArgs)
                    (hoistTransport (lift . lift) transport)
                    kademliaInst
                    allPlugins
                    currentParams ()
    else
        logError $ "You try to run wallet, but code wasn't compiled with wallet flag"
  where
    convPlugins = (,mempty) . map (\act -> ActionSpec $ \__vI __sA -> act)

#ifdef WITH_WEB
plugins ::
    ( SscConstraint ssc
    , WorkMode ssc m
    , MonadNodeContext ssc m
    ) => Args -> [m ()]
plugins Args {..}
    | enableWeb = [serveWebBase webPort]
    | otherwise = []
#endif

#ifdef WITH_WEB
pluginsGT ::
    ( WorkMode SscGodTossing m
    , MonadNodeContext SscGodTossing m
    ) => Args -> [m ()]
pluginsGT Args {..}
    | enableWeb = [serveWebGT webPort]
    | otherwise = []
#endif

utwProd :: SscConstraint ssc =>([WorkerSpec (ProductionMode ssc)], OutSpecs)
utwProd = first (map liftPlugin) updateTriggerWorker
  where
    liftPlugin (ActionSpec p) = ActionSpec $ \vI sa ->
        lift . p vI $ hoistSendActions getNoStatsT lift sa

utwStats :: SscConstraint ssc => ([WorkerSpec (StatsMode ssc)], OutSpecs)
utwStats = first (map liftPlugin) updateTriggerWorker
  where
    liftPlugin (ActionSpec p) = ActionSpec $ \vI sa -> do
        s <- getStatsMap
        lift . p vI $ hoistSendActions (runStatsT' s) lift sa

updateTriggerWorker
    :: SscConstraint ssc
    => ([WorkerSpec (RawRealModeK ssc)], OutSpecs)
updateTriggerWorker = first pure $ worker mempty $ \_ -> do
    logInfo "Update trigger worker is locked"
    void $ takeMVar =<< Ether.asks' ucUpdateSemaphore
    triggerShutdown

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

main :: IO ()
main = do
    printFlags
    args <- getNodeOptions
    let baseParams = getBaseParams "node" args
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
        in  foreverRejoinNetwork kademliaInstance (action kademliaInstance args transport')
