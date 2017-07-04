{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Universum

import qualified Data.ByteString.Char8      as BS8 (unpack)
import           Data.Maybe                 (fromJust)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Data.Time.Units            (toMicroseconds)
import qualified Ether
import           Formatting                 (sformat, shown, (%))
import           Mockable                   (Production, currentTime)
import           Network.Transport.Abstract (Transport, hoistTransport)
import qualified Network.Transport.TCP      as TCP (TCPAddr (..), TCPAddrInfo (..))
import           Serokell.Util              (sec)
import           System.Wlog                (logInfo)

import           Pos.Binary                 ()
import qualified Pos.CLI                    as CLI
import           Pos.Communication          (OutSpecs, WorkerSpec, worker, wrapActionSpec)
import           Pos.Core.Types             (Timestamp (..))
import           Pos.DHT.Workers            (dhtWorkers)
import           Pos.Discovery              (DiscoveryContextSum (..))
#ifndef DEV_MODE
import           Pos.Genesis                (genesisStakeDistribution)
#endif
import           Pos.DHT.Real               (KademliaDHTInstance (..),
                                             foreverRejoinNetwork)
import           Pos.Launcher               (NodeParams (..), bracketResourcesKademlia,
                                             runNodeReal)
import           Pos.Shutdown               (triggerShutdown)
import           Pos.Ssc.Class              (SscConstraint)
import           Pos.Ssc.GodTossing         (SscGodTossing)
import           Pos.Types                  (Timestamp (Timestamp))
import           Pos.Update.Context         (ucUpdateSemaphore)
import           Pos.Util                   (inAssertMode, mconcatPair)
import           Pos.Util.UserSecret        (usVss)
import           Pos.Util.Util              (powerLift)
import           Pos.WorkMode               (RealMode, WorkMode)

import           Pos.Explorer.Socket        (NotifierSettings (..))
import           Pos.Explorer.Web           (explorerPlugin, notifierPlugin)

import           ExplorerOptions            (Args (..), getExplorerOptions)
import           Params                     (getBaseParams, getKademliaParams,
                                             getNodeParams, gtSscParams)

-- Note: for now Kademlia discovery is hardcoded.

action
    :: KademliaDHTInstance
    -> Args
    -> (forall ssc . Transport (RealMode ssc))
    -> Production ()
action kad args@Args {..} transport = do
    systemStart <- getNodeSystemStart $ CLI.sysStart commonArgs
    logInfo $ sformat ("System start time is " % shown) systemStart
    t <- currentTime
    logInfo $ sformat ("Current time is " % shown) (Timestamp t)
    currentParams <- getNodeParams args systemStart

    putText "Running using GodTossing"
    let wDhtWorkers 
            :: WorkMode SscGodTossing m 
            => ([WorkerSpec m], OutSpecs)
            -> ([WorkerSpec m], OutSpecs)
        wDhtWorkers workers = runWithLogging workers

    let plugins = mconcatPair
            [ explorerPlugin webPort
            , notifierPlugin NotifierSettings{ nsPort = notifierPort }
            , wDhtWorkers $ lDhtWorkers kad
            , updateTriggerWorker
            ]

    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
    let gtParams = gtSscParams args vssSK

    runNodeReal @SscGodTossing
        (DCKademlia kad)
        transport
        plugins
        currentParams
        gtParams
  where
    lDhtWorkers
        :: WorkMode SscGodTossing m 
        => KademliaDHTInstance 
        -> ([WorkerSpec m], OutSpecs)
    lDhtWorkers kDHTInstance = dhtWorkers kDHTInstance

    -- TODO: What's the point of this?
    -- runWhileNotRecovering
    --     :: ([WorkerSpec m], OutSpecs)
    --     -> ([WorkerSpec m], OutSpecs)
    -- runWhileNotRecovering (ws, outs) = (map (fst . recoveryCommGuard . (, outs)) ws, outs)

    runWithLogging 
        :: WorkMode SscGodTossing m 
        => ([WorkerSpec m], OutSpecs)
        -> ([WorkerSpec m], OutSpecs)
    runWithLogging workers = first (map $ wrapActionSpec $ "worker" <> "dht") $ workers

updateTriggerWorker
    :: SscConstraint ssc
    => ([WorkerSpec (RealMode ssc)], OutSpecs)
updateTriggerWorker = first pure $ worker mempty $ \_ -> do
    logInfo "Update trigger worker is locked"
    void $ takeMVar =<< Ether.asks' ucUpdateSemaphore
    triggerShutdown

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

printFlags :: IO ()
printFlags = do
#ifdef DEV_MODE
    putText "[Attention] We are in DEV mode"
#else
    putText "[Attention] We are in PRODUCTION mode"
#endif
    inAssertMode $ putText "Asserts are ON"

main :: IO ()
main = do
    args <- getExplorerOptions
    printFlags
    let baseParams = getBaseParams "node" args
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
        in  foreverRejoinNetwork kademliaInstance (action kademliaInstance args transport')
