{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import           Control.Exception (throwIO)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (when, forM, void)
import qualified Data.ByteString.Char8 as B8
import           Data.Semigroup ((<>))
import qualified Network.Broadcast.OutboundQueue as OQ
import qualified Network.Broadcast.OutboundQueue.Types as OQ
import           Network.Transport (EndPointAddress (..))
import           Node (NodeId (..))
import           Options.Applicative
import           System.Posix.Signals
import qualified System.Wlog as Wlog
import           System.IO.Error (userError)

import           Pos.Core (HeaderHash)
import           Pos.Diffusion.Subscription.Common (withNetworkSubscription)
import           Pos.Diffusion.Types (Diffusion)
import qualified Pos.Diffusion.Types as Diffusion
import           Pos.Diffusion.Full
import qualified Pos.Logic.Types as Logic
import           Pos.Logic.Pure (pureLogic)
import qualified Pos.Network.Policy as Policy
import           Pos.Reporting.Health.Types (HealthStatus (..))
import           Pos.Util.Timer (newTimer)
import           Pos.Util.Trace (Severity (..), traceWith, wlogTrace)

import           Bench.Pos.Diffusion.Download.Common

data ClientOptions = ClientOptions
    { clientCommonOptions :: CommonOptions
    , clientBatch         :: Bool
    , clientN             :: Int
    , clientServerAddress :: String
    }

parseClientOptions :: Parser ClientOptions
parseClientOptions =
    ClientOptions <$> parseCommonOptions
                  <*> parseBatch
                  <*> parseN
                  <*> parseServerAddress

parseBatch :: Parser Bool
parseBatch = switch (long "batch")

parseN :: Parser Int
parseN = option auto (short 'n')

parseServerAddress :: Parser String
parseServerAddress = strOption (long "server-address")

someHash :: HeaderHash
someOtherHash :: HeaderHash
((!someHash) : (!someOtherHash) : _) = bunchOfHashes

-- 'Int' parameter is how many batches to download.
-- Number of blocks is this number multiplied by the batch size from common
-- options (which the server and client *must* agree upon).
batchDownload :: Monad m => Diffusion m -> NodeId -> Int -> m [Int]
batchDownload client serverAddress n = forM [1..n] $ \_ -> do
    -- Forcing the list and taking the length is realistic. In practice
    -- the next batch does not proceed until after this batch is verified and
    -- applied, which requires traversing the list.
    blocks <- Diffusion.getBlocks client serverAddress someHash [someOtherHash]
    pure $! length blocks

-- 'Int' parameter is how many *blocks* (not batches) to download.
streamDownload :: Diffusion IO -> NodeId -> Int -> IO ()
streamDownload client serverAddress n =
    Diffusion.streamBlocks client serverAddress someHash [someOtherHash] (loop n)
  where
    loop 0 _ = pure ()
    loop !n tbqueue = do
        next <- STM.atomically $ STM.readTBQueue tbqueue
        case next of
            -- Server has an infinite stream.
            Diffusion.StreamEnd -> throwIO (userError "unexpected early stream end")
            Diffusion.StreamBlock _ -> loop (n-1) tbqueue

subscribe sendActions serverAddress =
    withNetworkSubscription before middle keepalive after sendActions serverAddress
  where
    before    = const (pure ())
    middle    = const (pure ())
    keepalive = const (threadDelay 20000000)
    after     = const (const (const (pure ())))

main :: IO ()
main = do
    opts <- execParser (info parseClientOptions fullDesc)
    when (coShowLogs (clientCommonOptions opts)) enableLogging
    let host = coHost (clientCommonOptions opts)
        port = coPort (clientCommonOptions opts)
        batchSize = coBatchSize (clientCommonOptions opts)
        streamWindow = coStreamWindow (clientCommonOptions opts)
        batch = clientBatch opts
        n = clientN opts
        defaultPort = coDefaultPort (clientCommonOptions opts)
        serverAddress = NodeId (EndPointAddress (B8.pack (clientServerAddress opts)))
        fdconf = FullDiffusionConfiguration
            { fdcProtocolMagic = protocolMagic
            , fdcProtocolConstants = protocolConstants
            , fdcRecoveryHeadersMessage = batchSize
            , fdcStreamWindow = streamWindow
            , fdcLastKnownBlockVersion = blockVersion
            , fdcConvEstablishTimeout = 15000000
            , fdcTrace = wlogTrace ("client" <> "diffusion")
            }
        logic = pureLogic
            { Logic.getLcaMainChain = pure
            }
    oq <- OQ.new (wlogTrace ("client" <> "outboundqueue"))
                 Policy.defaultEnqueuePolicyEdgeBehindNat
                 (const (OQ.Dequeue OQ.NoRateLimiting (OQ.MaxInFlight maxBound)))
                 Policy.defaultFailurePolicyAuxx
                 (const OQ.BucketSizeUnlimited)
                 (OQ.UnknownNodeType (const OQ.NodeRelay))
    bracketTransport 15000000 host port $ \transport -> do
        (diffusion, runInternals) <- diffusionLayerFullExposeInternals
            fdconf
            transport
            oq
            defaultPort
            Nothing
            Nothing
            Nothing
            (pure (HSHealthy ""))
            Nothing
            logic
        runFullDiffusionInternals runInternals $ \internals ->
            subscribe (fdiSendActions internals) serverAddress $ do
                let logTrace = wlogTrace ("client")
                traceWith logTrace (Notice, "Begin download")
                if batch
                then void $ batchDownload diffusion serverAddress (fromIntegral n)
                else void $ streamDownload diffusion serverAddress (fromIntegral n * fromIntegral batchSize)
                traceWith logTrace (Notice, "End download")
