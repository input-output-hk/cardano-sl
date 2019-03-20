{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}

module Cardano.Node.NodeStateAdaptor (
    WithNodeState     -- opaque
  , NodeStateAdaptor  -- opaque
  , withNodeState
  , newNodeStateAdaptor
  , NodeConstraints
    -- * Additional types
  , SecurityParameter(..)
  , UnknownEpoch(..)
  , MissingBlock(..)
    -- * Specific queries
  , getTipSlotId
  , getSecurityParameter
  , getMaxTxSize
  , getFeePolicy
  , getSlotCount
  , getCoreConfig
  , getSlotStart
  , getNextEpochSlotDuration
  , getNodeSyncProgress
  , curSoftwareVersion
  , compileInfo
  ) where

import           Universum

import           Control.Lens (lens, to)
import           Control.Monad.STM (orElse)
import           Data.Time.Units (Millisecond)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Chain.Block (Block, HeaderHash, LastKnownHeader,
                     LastKnownHeaderTag, MainBlock, blockHeader, headerHash,
                     mainBlockSlot, prevBlockL)
import           Pos.Chain.Genesis as Genesis (Config (..), GenesisHash (..),
                     configEpochSlots, configK)
import           Pos.Chain.Update (HasUpdateConfiguration, SoftwareVersion,
                     bvdMaxTxSize, bvdTxFeePolicy)
import qualified Pos.Chain.Update as Upd
import           Pos.Context (NodeContext (..))
import           Pos.Core (BlockCount, SlotCount, Timestamp (..), TxFeePolicy,
                     difficultyL, getChainDifficulty)
import           Pos.Core.Slotting (EpochIndex (..), HasSlottingVar (..),
                     LocalSlotIndex (..), MonadSlots (..), SlotId (..))
import qualified Pos.DB.Block as DB
import           Pos.DB.BlockIndex (getTipHeader)
import           Pos.DB.Class (MonadDBRead (..), getBlock)
import           Pos.DB.GState.Lock (StateLock, withStateLockNoMetrics)
import           Pos.DB.Rocks (NodeDBs, dbGetDefault, dbIterSourceDefault)
import           Pos.DB.Update (UpdateContext, getAdoptedBVData)
import           Pos.Infra.Shutdown.Class (HasShutdownContext (..))
import qualified Pos.Infra.Slotting.Impl.Simple as S
import qualified Pos.Infra.Slotting.Util as Slotting
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Node.API (SecurityParameter (..))
import           Pos.Util (CompileTimeInfo, HasCompileInfo, HasLens (..),
                     lensOf')
import qualified Pos.Util as Util
import           Pos.Util.Concurrent.PriorityLock (Priority (..))
import           Pos.Util.Wlog (CanLog (..), HasLoggerName (..))

{-------------------------------------------------------------------------------
  Additional types
-------------------------------------------------------------------------------}



-- | Returned by 'getSlotStart' when requesting info about an unknown epoch
data UnknownEpoch = UnknownEpoch SlotId

-- | Thrown if we cannot find a previous block
--
-- If this ever happens it indicates a serious problem: the blockchain as
-- stored in the node is not correct.
data MissingBlock = MissingBlock CallStack HeaderHash
  deriving (Show)

instance Exception MissingBlock

{-------------------------------------------------------------------------------
  Locking
-------------------------------------------------------------------------------}

-- | Do we need to take the lock?
--
-- DB locks are important for consistency, but of course come with provisos.
-- In particular, we should not take a lock when we already have it (e.g.,
-- in 'MonadBListener'). Code that needs a lock but is unaware of the context
-- in which it is run can pass 'LockContext' up to the caller:
--
-- > foo :: MonadDBReadAdaptor m -> LockContext -> m ..
-- > foo db lc = withMonadDBRead db $ \withLock -> do
-- >     ..
-- >     x <- withLock lc LowPriority $ \tip -> ..
-- >     ..
--
-- which would then be called as
--
-- > foo db NotYetLocked
--
-- or
--
-- > foo db AlreadyLocked
data LockContext = AlreadyLocked | NotYetLocked

-- | Take the state lock (cf. 'withStateLockNoMetrics', 'ncStateLock').
--
-- NOTE: We always take the lock with low priority.
type Lock m = forall a. LockContext -> (HeaderHash -> m a) -> m a

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

-- | Node constraints
--
-- The underlying node uses a bunch of constraints that must be in scope.
-- We are slowly getting rid of these from the core, but until that is
-- complete, we must support them there. When they are replaced by regular
-- values, those values can be added to the 'Res' environment itself.
--
-- When using the 'WithStateMonad' it can occassionally be useful to define
-- local helper functions; these will typically have type
--
-- > NodeConstraints => WithNodeState ...
--
-- Using 'NodeConstraints' in such functions isolates these functions from
-- changes to the type classes used in the underlying node.
type NodeConstraints = (
      HasUpdateConfiguration
    , HasCompileInfo
    )

-- | Internal: node resources and reified type class dictionaries
data Res = forall ext. Res !(NodeResources ext)

-- | Monad in which the underlying node state is available
--
-- Use sparingly! The wallet keeps its own state, which at times may be
-- inconsistent with the node state. This should only be used for things like
-- wallet restoration.
--
-- NOTE: Although the 'MonadReader' instance is exposed, it should not
-- be relied on. We expose it because core monad definitions require
-- that this 'MonadReader' instance is present.
newtype WithNodeState m a = Wrap {unwrap :: ReaderT Res m a}
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadCatch
    , MonadThrow
    , MonadIO
    , MonadTrans
    , MonadReader Res
    )

-- NOTE type (WithLogger m) = (CanLog m, HasLoggerName m)

instance (MonadIO m) => CanLog (WithNodeState m) where
    dispatchMessage name severity =
        liftIO . dispatchMessage name severity

instance (Monad m) => HasLoggerName (WithNodeState m) where
    askLoggerName    = return "node"
    modifyLoggerName = flip const


-- | The 'NodeStateAdaptor' allows to bring the node state into scope
-- without polluting all the types in the kernel.
--
-- At the moment this is only partly mockable: calling 'withNodeState' is
-- not mockable, but the remainder of the functions are. This is a pragmatic
-- approach, and allows us to mock just what we need for the tests. In an
-- ideal world 'withNodeState' would eventually disappear.
--
-- See 'newNodeStateAdaptor'.
data NodeStateAdaptor m = Adaptor {
      -- | Run any action in the 'WithNodeState' monad
      --
      -- Warning: this is /not/ mockable. If this gets run from tests where
      -- a full node state is unavailable, this will throw an error.
      withNodeState :: forall a.
                       (    NodeConstraints
                         => Lock (WithNodeState m)
                         -> WithNodeState m a
                       )
                    -> m a

      -- | Get slot ID of current tip
      --
      -- Tests must pass in an explicit value here.
    , getTipSlotId :: m SlotId

      -- | Get maximum transaction size
    , getMaxTxSize :: m Byte

      -- | Get fee policy
    , getFeePolicy :: m TxFeePolicy

      -- | Get the security parameter (@k@)
    , getSecurityParameter :: m SecurityParameter

      -- | Get number of slots per epoch
      --
      -- This can be used as an input to 'flattenSlotIdExplicit'.
      --
      -- NOTE: If this constant ever changes, then we'd have to return something
      -- more detailed here ("slot count was X between epoch A and B, and Y
      -- thereafter"). However, the same change will have to be made to
      -- 'flattenSlotIdExplicit' in core as well as, probably, a ton of other
      -- places.
    , getSlotCount :: m SlotCount

    -- | Get the @Genesis.Config@
    , getCoreConfig :: m Genesis.Config

      -- | Get the start of a slot
      --
      -- When looking up data for past of the current epochs, the value should
      -- be known.
    , getSlotStart :: SlotId -> m (Either UnknownEpoch Timestamp)

      -- | Get last known slot duration.
    , getNextEpochSlotDuration :: m Millisecond

      -- | Get the "sync progress". This term is desperately overloaded but
      -- in this context we need something very simple: a tuple containing the
      -- "global blockchain height" and the "node blockchain height". The
      -- former is the maximum between the biggest height we observed from an
      -- unsolicited block we received and the current local tip:
      --
      -- global_height = max (last_known_header, node_local_tip)
      --
      -- The latter is simply the node local tip, i.e. "how far we went into
      -- chasing the global blockchain height during syncing".
    , getNodeSyncProgress :: LockContext -> m (Maybe BlockCount, BlockCount)

      -- | Version of application (code running)
    , curSoftwareVersion :: m SoftwareVersion

      -- | Git revision
    , compileInfo :: m CompileTimeInfo

    }

{-------------------------------------------------------------------------------
  Internal: lenses required for default monad instances

  NOTE: It is rather dubious that we have a 'MonadReader' over the
  'NodeResources' but then the core default monad instances require a /lens/.
  It doesn't make much sense. However, this is the pattern throughout the core
  codebase. A fight for another day.
-------------------------------------------------------------------------------}

mkResLens :: (forall ext. Lens' (NodeResources ext) a) -> Lens' Res a
mkResLens l = lens (\(Res nr)   -> nr ^. l)
                   (\(Res nr) a -> Res (nr & l .~ a))

nrContextLens :: Lens' (NodeResources ext) NodeContext
nrContextLens = \f nr -> (\x -> nr {nrContext = x}) <$> f (nrContext nr)

nrDBsLens :: Lens' (NodeResources ext) NodeDBs
nrDBsLens = \f nr -> (\x -> nr {nrDBs = x}) <$> f (nrDBs nr)

instance HasLens NodeDBs Res NodeDBs where
    lensOf = mkResLens nrDBsLens

instance HasLens StateLock Res StateLock where
    lensOf = mkResLens (nrContextLens . lensOf')

instance HasLens S.SimpleSlottingStateVar Res S.SimpleSlottingStateVar where
    lensOf = mkResLens (nrContextLens . lensOf')

instance HasLens UpdateContext Res UpdateContext where
    lensOf = mkResLens (nrContextLens . lensOf')

instance HasLens LastKnownHeaderTag Res LastKnownHeader where
    lensOf = mkResLens (nrContextLens . lensOf @LastKnownHeaderTag)

instance HasSlottingVar Res where
    slottingTimestamp = mkResLens (nrContextLens . slottingTimestamp)
    slottingVar       = mkResLens (nrContextLens . slottingVar)

instance HasShutdownContext Res where
    shutdownContext = mkResLens (nrContextLens . shutdownContext)

{-------------------------------------------------------------------------------
  Monad instances

  NOTE: Although these instances require 'NodeConstraints', 'withNodeContext'
  will make sure that this is in scope.
-------------------------------------------------------------------------------}


instance ( NodeConstraints
         , MonadThrow m
         , MonadIO    m
         , MonadCatch m
         ) => MonadDBRead (WithNodeState m) where
  dbGet         = dbGetDefault
  dbIterSource  = dbIterSourceDefault
  dbGetSerBlock = DB.dbGetSerBlockRealDefault
  dbGetSerUndo  = DB.dbGetSerUndoRealDefault
  dbGetSerBlund  = DB.dbGetSerBlundRealDefault

instance MonadIO m => MonadSlots Res (WithNodeState m) where
  getCurrentSlot           = S.getCurrentSlotSimple
  getCurrentSlotBlocking   = S.getCurrentSlotBlockingSimple
  getCurrentSlotInaccurate = S.getCurrentSlotInaccurateSimple
  currentTimeSlotting      = S.currentTimeSlottingSimple

{-------------------------------------------------------------------------------
  Creating the adaptor
-------------------------------------------------------------------------------}

-- | Constructor for 'NodeStateAdaptor'
--
-- NOTE: This captures the node constraints in the closure so that the adaptor
-- can be used in a place where these constraints is not available.
newNodeStateAdaptor :: forall m ext. (NodeConstraints, MonadIO m, MonadMask m)
                    => Config
                    -> NodeResources ext
                    -> NodeStateAdaptor m
newNodeStateAdaptor genesisConfig nr = Adaptor
    { withNodeState            =            run
    , getTipSlotId             =            run $ \_lock -> defaultGetTipSlotId genesisHash
    , getMaxTxSize             =            run $ \_lock -> defaultGetMaxTxSize
    , getFeePolicy             =            run $ \_lock -> defaultGetFeePolicy
    , getSlotStart             = \slotId -> run $ \_lock -> defaultGetSlotStart slotId
    , getNextEpochSlotDuration =            run $ \_lock -> defaultGetNextEpochSlotDuration
    , getNodeSyncProgress      = \lockCtx -> run $ defaultSyncProgress lockCtx
    , getSecurityParameter     = return . SecurityParameter $ configK genesisConfig
    , getSlotCount             = return $ configEpochSlots genesisConfig
    , getCoreConfig            = return genesisConfig
    , curSoftwareVersion       = return $ Upd.curSoftwareVersion Upd.updateConfiguration
    , compileInfo              = return $ Util.compileInfo
    }
  where
    genesisHash = configGenesisHash genesisConfig
    run :: forall a.
           (    NodeConstraints
             => Lock (WithNodeState m)
             -> WithNodeState m a
           )
        -> m a
    run act = runReaderT (unwrap $ act withLock) (Res nr)

-- | Internal wrapper around 'withStateLockNoMetrics'
--
-- NOTE: If we wanted to use 'withStateLock' instead we would need to
-- capture additional node context.
withLock :: (NodeConstraints, MonadMask m, MonadIO m) => Lock (WithNodeState m)
withLock AlreadyLocked f = headerHash <$> getTipHeader >>= f
withLock NotYetLocked  f = Wrap $ withStateLockNoMetrics LowPriority
                                $ unwrap . f

{-------------------------------------------------------------------------------
  Default implementations for functions that are mockable
-------------------------------------------------------------------------------}

defaultGetMaxTxSize :: (MonadIO m, MonadCatch m, NodeConstraints)
                    => WithNodeState m Byte
defaultGetMaxTxSize = bvdMaxTxSize <$> getAdoptedBVData

defaultGetFeePolicy :: (MonadIO m, MonadCatch m, NodeConstraints)
                    => WithNodeState m TxFeePolicy
defaultGetFeePolicy = bvdTxFeePolicy <$> getAdoptedBVData

-- | Get the slot ID of the chain tip
--
-- Returns slot 0 in epoch 0 if there are no blocks yet.
defaultGetTipSlotId :: (MonadIO m, MonadCatch m, NodeConstraints)
                    => GenesisHash -> WithNodeState m SlotId
defaultGetTipSlotId genesisHash = do
    hdrHash <- headerHash <$> getTipHeader
    aux <$> mostRecentMainBlock genesisHash hdrHash
  where
    aux :: Maybe MainBlock -> SlotId
    aux (Just mainBlock) = mainBlock ^. mainBlockSlot
    aux Nothing          = SlotId (EpochIndex 0) (UnsafeLocalSlotIndex 0)

-- | Get the start of the specified slot
defaultGetSlotStart :: MonadIO m
                    => SlotId -> WithNodeState m (Either UnknownEpoch Timestamp)
defaultGetSlotStart slotId =
    maybe (Left (UnknownEpoch slotId)) Right <$> Slotting.getSlotStart slotId

defaultGetNextEpochSlotDuration :: MonadIO m => WithNodeState m Millisecond
defaultGetNextEpochSlotDuration = Slotting.getNextEpochSlotDuration

defaultSyncProgress :: (MonadIO m, MonadMask m, NodeConstraints)
                    => LockContext
                    -> Lock (WithNodeState m)
                    -> WithNodeState m (Maybe BlockCount, BlockCount)
defaultSyncProgress lockContext lock = do
    (globalHeight, localHeight) <- lock lockContext $ \_localTipHash -> do
        -- We need to grab the localTip again as '_localTip' has type
        -- 'HeaderHash' but we cannot grab the difficulty out of it.
        headerRef <- view (lensOf @LastKnownHeaderTag)
        localTip  <- getTipHeader
        mbHeader <- atomically $ readTVar headerRef `orElse` pure Nothing
        pure (view (difficultyL . to getChainDifficulty) <$> mbHeader
             ,view (difficultyL . to getChainDifficulty) localTip
             )
    return (max localHeight <$> globalHeight, localHeight)

-- | Get the most recent main block starting at the specified header
--
-- Returns nothing if there are no (regular) blocks on the blockchain yet.
mostRecentMainBlock :: forall m. (MonadIO m, MonadCatch m, NodeConstraints)
                    => GenesisHash -> HeaderHash -> WithNodeState m (Maybe MainBlock)
mostRecentMainBlock genesisHash = go
  where
    go :: HeaderHash -> WithNodeState m (Maybe MainBlock)
    go hdrHash
      | hdrHash == getGenesisHash genesisHash = return Nothing
      | otherwise = do
          block <- getBlockOrThrow hdrHash
          case block of
            Right mainBlock ->
              return $ Just mainBlock
            Left _ebb -> do
              -- We found an EBB. We get the previous block and loop.
              --
              -- It is possible, in theory, that the previous block will again
              -- be an EBB, if there were no regular blocks in this epoch at
              -- all. In that case, we keep looking. Indeed, we may reach
              -- the genesis block, in which case no regular blocks exist on
              -- the block chain at all. We assume that only regular blocks
              -- can change the block version data (@bv@, @bvd@) and software
              -- version (@sv@), so that these two values remain valid
              -- throughout this search.
               go (block ^. blockHeader ^. prevBlockL)

    getBlockOrThrow :: HeaderHash -> WithNodeState m Block
    getBlockOrThrow hdrHash = do
        mBlock <- getBlock genesisHash hdrHash
        case mBlock of
          Nothing    -> throwM $ MissingBlock callStack hdrHash
          Just block -> return block

