{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import           Universum

import           Control.Monad (when)
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup ((<>))
import qualified Network.Broadcast.OutboundQueue as OQ
import qualified Network.Broadcast.OutboundQueue.Types as OQ
import           Options.Applicative
import           Pipes (yield)
import           System.Posix.Signals

import           Pos.Core (getBlockHeader, headerHash)
import           Pos.Diffusion.Full
import qualified Pos.Logic.Types as Logic
import           Pos.Logic.Pure (pureLogic)
import qualified Pos.Network.Policy as Policy
import           Pos.Reporting.Health.Types (HealthStatus (..))
import           Pos.Util.Chrono (NewestFirst (..), OldestFirst (..))
import           Pos.Util.Trace (Severity (..), traceWith, wlogTrace)

import           Bench.Pos.Diffusion.Download.Common

data ServerOptions = ServerOptions
    { serverCommonOptions :: CommonOptions
    }

parseServerOptions :: Parser ServerOptions
parseServerOptions = ServerOptions <$> parseCommonOptions

main = do
    opts <- execParser (info parseServerOptions fullDesc)
    when (coShowLogs (serverCommonOptions opts)) enableLogging
    -- Server will be held open until SIGINT comes, then it will gracefully
    -- shut down.
    sigIntMVar <- newEmptyMVar
    let host = coHost (serverCommonOptions opts)
        port = coPort (serverCommonOptions opts)
        batchSize = coBatchSize (serverCommonOptions opts)
        streamWindow = coStreamWindow (serverCommonOptions opts)
        defaultPort = coDefaultPort (serverCommonOptions opts)
        logTrace = wlogTrace ("server" <> "diffusion")
        fdconf = FullDiffusionConfiguration
            { fdcProtocolMagic = protocolMagic
            , fdcProtocolConstants = protocolConstants
            , fdcRecoveryHeadersMessage = batchSize
            , fdcStreamWindow = streamWindow
            , fdcLastKnownBlockVersion = blockVersion
            , fdcConvEstablishTimeout = 15000000
            , fdcTrace = logTrace
            }
        sigIntHandler = CatchOnce (putMVar sigIntMVar ())
        -- Logic layer for the server supports batch and stream block
        -- downloading according to the parameters.
        seed = 0
        size = 4
        !someBlock  = generateBlock seed size
        !someHeader = getBlockHeader someBlock
        !someHash   = headerHash someHeader
        logic = pureLogic
            { Logic.getBlock = \_ -> pure (Just someBlock)
            , Logic.getBlockHeader = \_ -> pure (Just someHeader)
            , Logic.getHashesRange = \_ _ _ ->
                  pure (Right (OldestFirst (NE.fromList (replicate (fromIntegral batchSize) someHash))))
            , Logic.getBlockHeaders = \_ _ _ ->
                  pure (Right (NewestFirst (NE.fromList (replicate (fromIntegral batchSize) someHeader))))
            , Logic.streamBlocks = \_ -> forever (yield someBlock)
            }
    installHandler sigINT sigIntHandler Nothing
    -- A full diffusion layer needs an outbound queue, although we won't
    -- actually be using it in this server.
    oq <- OQ.new (wlogTrace ("server" <> "outboundqueue"))
                 Policy.defaultEnqueuePolicyRelay
                 (const (OQ.Dequeue OQ.NoRateLimiting (OQ.MaxInFlight maxBound)))
                 Policy.defaultFailurePolicyAuxx
                 (const OQ.BucketSizeUnlimited)
                 (OQ.UnknownNodeType (const OQ.NodeEdge))
    -- Create a TCP transport and run a full diffusion layer, holding it open
    -- until SIGINT.
    bracketTransport 15000000 host port $ \transport -> do
        (diffusion, runInternals) <- diffusionLayerFullExposeInternals
            fdconf
            transport
            oq
            defaultPort
            Nothing
            -- This parameter determines whether a subscription listener is
            -- put up (also affects the InSpecs/OutSpecs).
            (Just (OQ.NodeRelay, OQ.BucketSizeUnlimited))
            Nothing
            (pure (HSHealthy ""))
            Nothing
            logic
        runFullDiffusionInternals runInternals $ \_ -> do
            takeMVar sigIntMVar
            traceWith logTrace (Notice, "Got SIGINT")
    traceWith logTrace (Notice, "Goodbye!")
