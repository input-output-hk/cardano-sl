{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS -fno-cross-module-specialise #-}

module Main
       ( main
       ) where

import           Universum

import           Control.Monad.Trans        (MonadTrans)
import qualified Data.ByteString.Char8      as BS8 (unpack)
import           Data.Maybe                 (fromJust)
import qualified Data.Set                   as S (fromList)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Data.Time.Units            (toMicroseconds)
import qualified Ether
import           Formatting                 (sformat, shown, (%))
import           Mockable                   (Production, currentTime)
import           Network.Transport.Abstract (Transport, hoistTransport)
import qualified Network.Transport.TCP      as TCP (TCPAddr (..), TCPAddrInfo (..))
import           Serokell.Util              (sec)
import           System.Wlog                (logError, logInfo)

import           Pos.Binary                 ()
import qualified Pos.CLI                    as CLI
import           Pos.Communication          (ActionSpec (..), NodeId, OutSpecs,
                                             WorkerSpec, worker, wrapActionSpec)
import           Pos.Constants              (isDevelopment)
import           Pos.Context                (MonadNodeContext, recoveryCommGuard)
import           Pos.Core.Types             (Timestamp (..))
import           Pos.DHT.Real               (KademliaDHTInstance (..))
import           Pos.DHT.Workers            (dhtWorkers)
import           Pos.Discovery              (DiscoveryContextSum (..))
import           Pos.Launcher               (NodeParams (..), bracketResources,
                                             bracketResourcesKademlia, runNode,
                                             runNodeReal)
import           Pos.Security               (SecurityWorkersClass)
import           Pos.Shutdown               (triggerShutdown)
import           Pos.Ssc.Class              (SscConstraint, SscParams)
import           Pos.Ssc.GodTossing         (SscGodTossing)
import           Pos.Ssc.NistBeacon         (SscNistBeacon)
import           Pos.Ssc.SscAlgo            (SscAlgo (..))
import           Pos.Update.Context         (ucUpdateSemaphore)
import           Pos.Util                   (inAssertMode)
import           Pos.Util.TimeWarp          (addressToNodeId)
import           Pos.Util.UserSecret        (usVss)
import           Pos.Util.Util              (powerLift)
import           Pos.WorkMode               (RealMode, WorkMode)
#ifdef WITH_WEB
import           Pos.Wallet.Redirect        (WalletRedirects, liftWalletRedirects)
import           Pos.Web                    (serveWebGT)
#ifdef WITH_WALLET
import           Pos.Wallet.Web             (WalletRealWebMode, bracketWalletWS,
                                             bracketWalletWebDB, runWRealMode,
                                             walletServeWebFull, walletServerOuts)
#endif
#endif

import           NodeOptions                (Args (..), getNodeOptions)
import           Params                     (getBaseParams, getKademliaParams,
                                             getNodeParams, getPeersFromArgs, gtSscParams)

action
    :: Either KademliaDHTInstance (Set NodeId)
    -> Args
    -> (forall ssc . Transport (RealMode ssc))
    -> Production ()
action peerHolder args@Args {..} transport = do
    systemStart <- getNodeSystemStart $ CLI.sysStart commonArgs
    logInfo $ sformat ("System start time is " % shown) systemStart
    t <- currentTime
    logInfo $ sformat ("Current time is " % shown) (Timestamp t)
    currentParams <- getNodeParams args systemStart
    putText $ "Running using " <> show (CLI.sscAlgo commonArgs)
#ifdef WITH_WALLET
    putText $ "Is wallet enabled: " <> show enableWallet
#else
    putText "Wallet is disabled, because software is built w/o it"
#endif
    putText $ "Static peers is on: " <> show staticPeers

    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
    let gtParams = gtSscParams args vssSK
    let wDhtWorkers :: WorkMode ssc m => KademliaDHTInstance -> ([WorkerSpec m], OutSpecs)
        wDhtWorkers = (\(ws, outs) -> (map (fst . recoveryCommGuard . (, outs)) ws, outs)) . -- TODO simplify
                      first (map $ wrapActionSpec $ "worker" <> "dht") . dhtWorkers

#ifdef WITH_WEB
    when enableWallet $ do
        let currentPluginsGT :: (MonadNodeContext SscGodTossing m, WorkMode SscGodTossing m) => [m ()]
            currentPluginsGT = pluginsGT args
        bracketWalletWebDB walletDbPath walletRebuildDb $ \db ->
            bracketWalletWS $ \conn -> do
                let discoveryCtx = either DCKademlia DCStatic peerHolder
                let almostAllPlugins :: (MonadNodeContext SscGodTossing m, WorkMode SscGodTossing m)
                                     => ([WorkerSpec m], OutSpecs)
                    almostAllPlugins = either
                        (\ki -> mconcat [wDhtWorkers ki, convPlugins currentPluginsGT])
                        (const $ convPlugins currentPluginsGT)
                        peerHolder
                let allPlugins :: ([WorkerSpec WalletRealWebMode], OutSpecs)
                    allPlugins = almostAllPlugins <> walletProd args
                case CLI.sscAlgo commonArgs of
                    NistBeaconAlgo -> logError "Wallet does not support NIST beacon!"
                    GodTossingAlgo ->
                        runWRealMode db conn discoveryCtx transportW currentParams gtParams
                            (runNode @SscGodTossing Nothing allPlugins)
#endif
#ifdef WITH_WALLET
    let userWantsWallet = enableWallet
#else
    let userWantsWallet = False
#endif
    unless userWantsWallet $ do
        let sscParams :: Either (SscParams SscNistBeacon) (SscParams SscGodTossing)
            sscParams = bool (Left ()) (Right gtParams) (CLI.sscAlgo commonArgs == GodTossingAlgo)
        let discoveryCtx = either DCKademlia DCStatic peerHolder
        let plugins :: forall ssc .
                (SscConstraint ssc, SecurityWorkersClass ssc)
                => ([WorkerSpec (RealMode ssc)], OutSpecs)
            plugins = either
                (\kad -> mconcat [wDhtWorkers kad, updateTriggerWorker])
                (const updateTriggerWorker)
                peerHolder
        let runner :: forall ssc .
                (SscConstraint ssc, SecurityWorkersClass ssc)
                => SscParams ssc -> Production ()
            runner = runNodeReal @ssc discoveryCtx transport
                        (plugins @ssc) currentParams
        either (runner @SscNistBeacon) (runner @SscGodTossing) sscParams
#ifdef WITH_WEB
  where
    convPlugins = (,mempty) . map (\act -> ActionSpec $ \__vI __sA -> act)

    transportW ::
        forall ssc t0 t1 .
        ( Each '[MonadTrans] [t0, t1]
        , Each '[Monad] [ t0 (RealMode ssc)
                        , t1 (t0 (RealMode ssc))
                        ]
        )
        => Transport (WalletRedirects (t1 $ t0 $ RealMode ssc))
    transportW = hoistTransport (liftWalletRedirects . lift . lift) transport
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

updateTriggerWorker
    :: SscConstraint ssc
    => ([WorkerSpec (RealMode ssc)], OutSpecs)
updateTriggerWorker = first pure $ worker mempty $ \_ -> do
    logInfo "Update trigger worker is locked"
    void $ takeMVar =<< Ether.asks' ucUpdateSemaphore
    triggerShutdown

----------------------------------------------------------------------------
-- Wallet stuff
----------------------------------------------------------------------------

#ifdef WITH_WALLET

walletProd :: Args -> ([WorkerSpec WalletRealWebMode], OutSpecs)
walletProd Args {..} = first pure $ worker walletServerOuts $ \sendActions ->
    walletServeWebFull
        sendActions
        walletDebug
        walletPort

#endif

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
    args <- getNodeOptions
    printFlags
    let baseParams = getBaseParams "node" args
    if staticPeers args then do
        allPeers <- S.fromList . map addressToNodeId <$> getPeersFromArgs args
        bracketResources baseParams TCP.Unaddressable $ \transport -> do
            let transport' = hoistTransport
                    (powerLift :: forall ssc t . Production t -> RealMode ssc t)
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
                    (powerLift :: forall ssc t . Production t -> RealMode ssc t)
                    transport
            in action (Left kademliaInstance) args transport'
