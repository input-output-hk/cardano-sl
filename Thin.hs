-- A program which runs two diffusion layers over a TCP transport, and has
-- one of them request blocks from the other.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent (threadDelay)
import           Data.Reflection (give)
import           Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Network.Transport.TCP as TCP
import           Mockable.Production (Production, runProduction)
import           System.Wlog (usingLoggerName, logError, logInfo, LoggerNameBox)
import           Formatting (sformat, shown, (%))

import           Pos.Binary (biSize)
import           Pos.Communication.Limits.Given ()
import           Pos.Core (BlockVersionData, BlockVersion (..))
import           Pos.Core.Slotting (Timestamp (..))
import           Pos.DHT.Real (KademliaParams (..))
import           Pos.Network.Types (NetworkConfig (..))
import           Pos.Network.CLI (intNetworkConfigOpts, NetworkConfigOpts (..))
import           Pos.Diffusion.Full (diffusionLayerFull)
import           Pos.Diffusion.Transport.TCP (bracketTransportTCP)
import           Pos.Diffusion.Types (Diffusion (..), DiffusionLayer (..))
import           Pos.Logic.Pure (pureLogic, blockVersionData, blockHeader, mainBlockHeaderHash, block)

import           Pos.Launcher.Resource (loggerBracket)
import           Pos.Launcher (LoggingParams (..))
import           Pos.Launcher.Configuration (withConfigurations, ConfigurationOptions (..))

import           Pos.Util.TimeWarp (addressToNodeId)

main :: IO ()
main =
    loggerBracket lparams $
    runProduction $
    usingLoggerName "diffusion" $
    withConfigurations configurationOptions $
    give getBlockVersionData $ do
        networkConfig1 <- lift $ intNetworkConfigOpts networkConfigOpts1
        networkConfig2 <- lift $ intNetworkConfigOpts networkConfigOpts2
        let tcpAddr1 = ncTcpAddr networkConfig1
            tcpAddr2 = ncTcpAddr networkConfig2
        bracketTransportTCP tcpAddr1 $ \transport1 ->
            bracketTransportTCP tcpAddr2 $ \transport2 ->
            diffusionLayerFull networkConfig1 blockVersion transport1 Nothing $ \expectLogic1 ->
            diffusionLayerFull networkConfig2 blockVersion transport2 Nothing $ \expectLogic2 -> do
                diffusionLayer1 <- expectLogic1 (pureLogic 160000)
                diffusionLayer2 <- expectLogic2 (pureLogic 160000)
                let diffusion1 = diffusion diffusionLayer1
                -- Run 2 first because it's the server.
                runDiffusionLayer diffusionLayer2 $
                    runDiffusionLayer diffusionLayer1 $ do
                        -- Unfortunately, we have to wait for a subscription to
                        -- be made. 2 seconds should do it.
                        logInfo $ sformat ("Size of block is "%shown) (biSize block)
                        liftIO (threadDelay 2000000)
                        -- 420 bytes per block (they're empty blocks).
                        -- 2000 blocks per request (pure logic layer hardcoded).
                        -- => 840kb per request
                        -- 500 requests gives 1,000,000 blocks and 420Mb of
                        --   block data.
                        {-
                        let downloadBlocks 0 = pure ()
                            downloadBlocks n = do
                                blks <- getBlocks diffusion1 serverNodeId blockHeader [mainBlockHeaderHash]
                                case blks of
                                    Left err -> logError $ sformat shown err
                                    Right its -> do
                                        -- logInfo $ sformat shown (length its)
                                        downloadBlocks (n-1)
                        -}
                        start <- liftIO getCurrentTime
                        --downloadBlocks 8
                        streamBlocks diffusion1 blockHeader [mainBlockHeaderHash] $ \block -> pure ()
                        end <- liftIO getCurrentTime
                        logInfo $ sformat ("Retrieval elapsed wall-clock time is: "%shown) (diffUTCTime end start)
  where
    lparams = LoggingParams
        { lpHandlerPrefix = Nothing
        , lpConfigPath = Nothing
        , lpDefaultName = "diffusion"
        , lpConsoleLog = Just True
        }

networkConfigOpts1 :: NetworkConfigOpts
networkConfigOpts1 = NetworkConfigOpts
    { ncoTopology = Just "./topology-client.yaml"
    , ncoKademlia = Nothing
    , ncoSelf     = Just "client"
    , ncoPort     = 3000
    , ncoPolicies = Just "./policies-client.yaml"
    , ncoBindAddress = Nothing
    , ncoExternalAddress = Nothing
    }

networkConfigOpts2 :: NetworkConfigOpts
networkConfigOpts2 = NetworkConfigOpts
    { ncoTopology = Just "./topology-server.yaml"
    , ncoKademlia = Nothing
    , ncoSelf     = Just "server"
    , ncoPort     = 3000
    , ncoPolicies = Nothing
    , ncoBindAddress = Just ("127.0.0.1", 3001)
    , ncoExternalAddress = Just ("127.0.0.1", 3001)
    }

serverNodeId = addressToNodeId ("127.0.0.1", 3001)

-- Unfortunately [CSL-2141] we still need to bring in a bunch of configuration
-- stuff that's not relevant to the diffusion layer, because it's built-in to
-- serialization and smart constructors that we cannot avoid using.
-- So we just take it all, using a configuration file, as we do in the main
-- CSL program.
configurationOptions = ConfigurationOptions
    { cfoFilePath    = "./lib/configuration.yaml"
    , cfoKey         = "dev"
    , cfoSystemStart = Just (Timestamp 0)
    , cfoSeed        = Nothing
    }

getBlockVersionData :: LoggerNameBox Production BlockVersionData
getBlockVersionData = pure blockVersionData

blockVersion :: BlockVersion
blockVersion = BlockVersion
    { bvMajor = 0
    , bvMinor = 0
    , bvAlt   = 0
    }

tcpAddr :: TCP.TCPAddr
tcpAddr = TCP.Addressable $ TCP.TCPAddrInfo
    { TCP.tcpBindHost = "127.0.0.1"
    , TCP.tcpBindPort = "0"
    , TCP.tcpExternalAddress = (,) "127.0.0.1"
    }
