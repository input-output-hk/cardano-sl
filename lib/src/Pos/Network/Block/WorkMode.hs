{-# LANGUAGE DataKinds #-}

-- | Various constraints needed for block processing.

module Pos.Network.Block.WorkMode
    ( BlockInstancesConstraint
    , BlockWorkMode
    ) where

import           Universum

import           Data.Default (Default)

import           Pos.Binary.Class (Bi)
import           Pos.Chain.Block (HasBlockConfiguration, HasSlogContext,
                     LastKnownHeader, LastKnownHeaderTag)
import           Pos.Chain.Security (SecurityParams)
import           Pos.Core.Context (HasPrimaryKey)
import           Pos.Core.JsonLog (CanJsonLog)
import           Pos.DB.Block (LrcModeFull)
import           Pos.DB.Txp (GenericTxpLocalData, MempoolExt, MonadTxpLocal,
                     TxpHolderTag)
import           Pos.DB.Update (UpdateContext)
import           Pos.Infra.Communication.Protocol (Message)
import           Pos.Infra.Recovery.Info (MonadRecoveryInfo)
import           Pos.Infra.Recovery.Types (RecoveryHeader, RecoveryHeaderTag)
import           Pos.Infra.Shutdown.Class (HasShutdownContext)
import           Pos.Infra.StateLock (StateLock, StateLockMetrics)
import           Pos.Infra.Util.JsonLog.Events (MemPoolModifyReason)
import           Pos.Network.Block.RetrievalQueue (BlockRetrievalQueue,
                     BlockRetrievalQueueTag)
import           Pos.Network.Block.Types (MsgBlock, MsgGetBlocks, MsgGetHeaders,
                     MsgHeaders)
import           Pos.Util.Util (HasLens, HasLens')
import           Pos.Util.Wlog (WithLogger)

-- | These instances are implemented in @Pos.Binary.Communication@,
-- and @Pos.Communication.Limits@, which are unavailable at this
-- point, hence we defer providing them to the calling site.
type BlockInstancesConstraint =
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
    )

-- | A subset of @WorkMode@.
type BlockWorkMode ctx m =
    ( BlockInstancesConstraint

    , Default (MempoolExt m)

    , LrcModeFull ctx m
    , MonadRecoveryInfo ctx m
    , MonadTxpLocal m

    , HasPrimaryKey ctx
    , HasShutdownContext ctx
    , HasSlogContext ctx

    , HasLens BlockRetrievalQueueTag ctx BlockRetrievalQueue
    , HasLens LastKnownHeaderTag ctx LastKnownHeader
    , HasLens RecoveryHeaderTag ctx RecoveryHeader
    , HasLens TxpHolderTag ctx (GenericTxpLocalData (MempoolExt m))
    , HasLens' ctx SecurityParams
    , HasLens' ctx StateLock
    , HasLens' ctx (StateLockMetrics MemPoolModifyReason)
    , HasLens' ctx UpdateContext

    , CanJsonLog m
    , WithLogger m

    , HasBlockConfiguration
    )
