{-# LANGUAGE DataKinds #-}

-- | Various constraints needed for block processing.

module Pos.Block.BlockWorkMode
    ( BlockInstancesConstraint
    , BlockWorkMode
    ) where

import           Universum

import           Data.Default (Default)
import           Ether.Internal (HasLens)
import           Mockable (Catch, Delay, Fork, Mockables, SharedAtomic, Throw)
import           System.Wlog (WithLogger)

import           Pos.Binary.Class (Bi)
import           Pos.Block.Configuration (HasBlockConfiguration)
import           Pos.Block.Network.Types (MsgBlock, MsgGetBlocks, MsgGetHeaders, MsgHeaders)
import           Pos.Block.RetrievalQueue (BlockRetrievalQueue, BlockRetrievalQueueTag)
import           Pos.Block.Slog.Types (HasSlogContext)
import           Pos.Block.Types (LastKnownHeader, LastKnownHeaderTag, ProgressHeader,
                                  ProgressHeaderTag, RecoveryHeader, RecoveryHeaderTag)
import           Pos.Communication.Limits.Types (MessageLimited)
import           Pos.Communication.Protocol (Message)
import           Pos.Core.Context (HasPrimaryKey)
import           Pos.Lrc.Worker (LrcModeFull)
import           Pos.Recovery.Info (MonadRecoveryInfo)
import           Pos.Security.Params (SecurityParams)
import           Pos.Shutdown.Class (HasShutdownContext)
import           Pos.StateLock (StateLock, StateLockMetrics)
import           Pos.Txp (GenericTxpLocalData, MempoolExt, MonadTxpLocal, TxpHolderTag)
import           Pos.Update.Context (UpdateContext)
import           Pos.Util.TimeWarp (CanJsonLog)
import           Pos.Util.Util (HasLens')

-- | These instances are implemented in @Pos.Binary.Communication@,
-- @Pos.Communication.Message@ and @Pos.Communication.Limits@, which
-- are unavailable at this point, hence we defer providing them
-- to the calling site.
type BlockInstancesConstraint m =
    ( Each '[Bi]
        [ MsgGetHeaders
        , MsgHeaders
        , MsgGetBlocks
        , MsgBlock ]
    , Each '[Message]
        [ MsgGetHeaders
        , MsgHeaders
        , MsgGetBlocks
        , MsgBlock ]
    , MessageLimited MsgGetHeaders m
    , MessageLimited MsgHeaders m
    , MessageLimited MsgGetBlocks m
    , MessageLimited MsgBlock m
    )

-- | A subset of @WorkMode@.
type BlockWorkMode ctx m =
    ( BlockInstancesConstraint m

    , Default (MempoolExt m)
    , Mockables m [Catch, Delay, Fork, SharedAtomic, Throw]

    , LrcModeFull ctx m
    , MonadRecoveryInfo m
    , MonadTxpLocal m

    , HasPrimaryKey ctx
    , HasShutdownContext ctx
    , HasSlogContext ctx

    , HasLens BlockRetrievalQueueTag ctx BlockRetrievalQueue
    , HasLens LastKnownHeaderTag ctx LastKnownHeader
    , HasLens ProgressHeaderTag ctx ProgressHeader
    , HasLens RecoveryHeaderTag ctx RecoveryHeader
    , HasLens TxpHolderTag ctx (GenericTxpLocalData (MempoolExt m))
    , HasLens' ctx SecurityParams
    , HasLens' ctx StateLock
    , HasLens' ctx StateLockMetrics
    , HasLens' ctx UpdateContext

    , CanJsonLog m
    , WithLogger m

    , HasBlockConfiguration
    )
