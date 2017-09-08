{-# LANGUAGE TypeFamilies #-}

-- | Execution mode used in Rubbish.

module Mode
       (
         -- * Extra types
         CmdCtx (..)

       -- * Mode, context, etc.
       , RubbishContext (..)
       , RubbishMode
       , RubbishSscType

       -- * Helpers
       , getCmdCtx
       , realModeToRubbish
       ) where

import           Universum

import           Control.Lens             (makeLensesWith)
import           Control.Monad.Morph      (hoist)
import           Control.Monad.Reader     (withReaderT)
import           Mockable                 (Production)
import           System.Wlog              (HasLoggerName (..))

import           Pos.Block.BListener      (MonadBListener (..))
import           Pos.Block.Core           (Block, BlockHeader)
import           Pos.Block.Slog           (HasSlogContext (..), HasSlogGState (..))
import           Pos.Block.Types          (Undo)
import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Balances  (MonadBalances (..), getBalanceFromUtxo)
import           Pos.Client.Txp.History   (MonadTxHistory (..), getBlockHistoryDefault,
                                           getLocalHistoryDefault, saveTxDefault)
import           Pos.Communication        (NodeId)
import           Pos.Context              (HasNodeContext (..), unGenesisUtxo)
import           Pos.Core                 (HasCoreConstants, HasPrimaryKey (..), IsHeader)
import           Pos.Crypto               (PublicKey)
import           Pos.DB                   (MonadGState (..))
import           Pos.DB.Class             (MonadBlockDBGeneric (..),
                                           MonadBlockDBGenericWrite (..), MonadDB (..),
                                           MonadDBRead (..))
import           Pos.KnownPeers           (MonadFormatPeers (..), MonadKnownPeers (..))
import           Pos.Recovery             ()
import           Pos.Reporting            (HasReportingContext (..))
import           Pos.Shutdown             (HasShutdownContext (..))
import           Pos.Slotting.Class       (MonadSlots (..))
import           Pos.Slotting.MemState    (HasSlottingVar (..), MonadSlotsData)
import           Pos.Ssc.Class            (HasSscContext (..), SscBlock)
import           Pos.Ssc.GodTossing       (SscGodTossing)
import           Pos.Txp                  (filterUtxoByAddrs)
import           Pos.Util                 (Some (..))
import           Pos.Util.JsonLog         (HasJsonLogConfig (..))
import           Pos.Util.LoggerName      (HasLoggerName' (..))
import qualified Pos.Util.OutboundQueue   as OQ.Reader
import           Pos.Util.TimeWarp        (CanJsonLog (..))
import           Pos.Util.UserSecret      (HasUserSecret (..))
import           Pos.Util.Util            (HasLens (..), lensOf', postfixLFields)
import           Pos.WorkMode             (RealMode, RealModeContext (..))

import           Pos.Rubbish.Hacks        (makePubKeyAddressRubbish)

-- | Command execution context.
data CmdCtx = CmdCtx
    { ccPeers :: ![NodeId]
    }

type RubbishSscType = SscGodTossing

type RubbishMode = ReaderT RubbishContext Production

data RubbishContext = RubbishContext
    { rcRealModeContext :: !(RealModeContext RubbishSscType)
    , rcCmdCtx          :: !CmdCtx
    }

makeLensesWith postfixLFields ''RubbishContext

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Get 'CmdCtx' in 'RubbishMode'.
getCmdCtx :: RubbishMode CmdCtx
getCmdCtx = view rcCmdCtx_L

-- | Turn 'RealMode' action into 'RubbishMode' action.
realModeToRubbish :: RealMode RubbishSscType a -> RubbishMode a
realModeToRubbish = withReaderT rcRealModeContext

----------------------------------------------------------------------------
-- Boilerplate instances
----------------------------------------------------------------------------

instance HasSscContext RubbishSscType RubbishContext where
    sscContext = rcRealModeContext_L . sscContext

instance HasPrimaryKey RubbishContext where
    primaryKey = rcRealModeContext_L . primaryKey

instance HasReportingContext RubbishContext  where
    reportingContext = rcRealModeContext_L . reportingContext

instance HasUserSecret RubbishContext where
    userSecret = rcRealModeContext_L . userSecret

instance HasShutdownContext RubbishContext where
    shutdownContext = rcRealModeContext_L . shutdownContext

instance HasNodeContext RubbishSscType RubbishContext where
    nodeContext = rcRealModeContext_L . nodeContext

instance HasSlottingVar RubbishContext where
    slottingTimestamp = rcRealModeContext_L . slottingTimestamp
    slottingVar = rcRealModeContext_L . slottingVar

instance {-# OVERLAPPABLE #-}
    HasLens tag (RealModeContext RubbishSscType) r =>
    HasLens tag RubbishContext r
  where
    lensOf = rcRealModeContext_L . lensOf @tag

instance HasLoggerName' RubbishContext where
    loggerName = rcRealModeContext_L . loggerName

instance HasSlogContext RubbishContext where
    slogContext = rcRealModeContext_L . slogContext

instance HasSlogGState RubbishContext where
    slogGState = rcRealModeContext_L . slogGState

instance HasJsonLogConfig RubbishContext where
    jsonLogConfig = rcRealModeContext_L . jsonLogConfig

instance (HasCoreConstants, MonadSlotsData ctx RubbishMode)
      => MonadSlots ctx RubbishMode
  where
    getCurrentSlot = realModeToRubbish getCurrentSlot
    getCurrentSlotBlocking = realModeToRubbish getCurrentSlotBlocking
    getCurrentSlotInaccurate = realModeToRubbish getCurrentSlotInaccurate
    currentTimeSlotting = realModeToRubbish currentTimeSlotting

instance {-# OVERLAPPING #-} HasLoggerName RubbishMode where
    getLoggerName = realModeToRubbish getLoggerName
    modifyLoggerName f action = do
        cmdCtx <- getCmdCtx
        let rubbishToRealMode :: RubbishMode a -> RealMode RubbishSscType a
            rubbishToRealMode = withReaderT (flip RubbishContext cmdCtx)
        realModeToRubbish $ modifyLoggerName f $ rubbishToRealMode action

instance {-# OVERLAPPING #-} CanJsonLog RubbishMode where
    jsonLog = realModeToRubbish ... jsonLog

instance MonadDBRead RubbishMode where
    dbGet = realModeToRubbish ... dbGet
    dbIterSource tag p = hoist (hoist realModeToRubbish) (dbIterSource tag p)

instance MonadDB RubbishMode where
    dbPut = realModeToRubbish ... dbPut
    dbWriteBatch = realModeToRubbish ... dbWriteBatch
    dbDelete = realModeToRubbish ... dbDelete

instance HasCoreConstants =>
         MonadBlockDBGenericWrite (BlockHeader RubbishSscType) (Block RubbishSscType) Undo RubbishMode where
    dbPutBlund = realModeToRubbish ... dbPutBlund

instance HasCoreConstants =>
         MonadBlockDBGeneric (BlockHeader RubbishSscType) (Block RubbishSscType) Undo RubbishMode
  where
    dbGetBlock  = realModeToRubbish ... dbGetBlock
    dbGetUndo   = realModeToRubbish ... dbGetUndo @(BlockHeader RubbishSscType) @(Block RubbishSscType) @Undo
    dbGetHeader = realModeToRubbish ... dbGetHeader @(BlockHeader RubbishSscType) @(Block RubbishSscType) @Undo

instance HasCoreConstants =>
         MonadBlockDBGeneric (Some IsHeader) (SscBlock RubbishSscType) () RubbishMode
  where
    dbGetBlock  = realModeToRubbish ... dbGetBlock
    dbGetUndo   = realModeToRubbish ... dbGetUndo @(Some IsHeader) @(SscBlock RubbishSscType) @()
    dbGetHeader = realModeToRubbish ... dbGetHeader @(Some IsHeader) @(SscBlock RubbishSscType) @()

instance MonadGState RubbishMode where
    gsAdoptedBVData = realModeToRubbish ... gsAdoptedBVData

instance HasCoreConstants => MonadBListener RubbishMode where
    onApplyBlocks = realModeToRubbish ... onApplyBlocks
    onRollbackBlocks = realModeToRubbish ... onRollbackBlocks

-- FIXME: I preserved the old behavior, but it most likely should be
-- changed!
instance HasCoreConstants => MonadBalances RubbishMode where
    getOwnUtxos addrs = filterUtxoByAddrs addrs . unGenesisUtxo <$> view lensOf'
    getBalance = getBalanceFromUtxo

instance HasCoreConstants => MonadTxHistory RubbishSscType RubbishMode where
    getBlockHistory = getBlockHistoryDefault @RubbishSscType
    getLocalHistory = getLocalHistoryDefault
    saveTx = saveTxDefault

instance MonadKnownPeers RubbishMode where
    updatePeersBucket = realModeToRubbish ... updatePeersBucket

instance MonadFormatPeers RubbishMode where
    formatKnownPeers = OQ.Reader.formatKnownPeersReader (rmcOutboundQ . rcRealModeContext)

instance MonadAddresses RubbishMode where
    type AddrData RubbishMode = PublicKey
    getNewAddress = makePubKeyAddressRubbish
