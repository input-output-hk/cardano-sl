{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cardano.Wallet.Kernel.NodeStateAdaptor (
    WithNodeState     -- opaque
  , NodeStateAdaptor  -- opaque
  , withNodeState
  , newNodeStateAdaptor
  , NodeConstraints
    -- * Locking
  , Lock
  , LockContext(..)
    -- * Specific queries
  , mostRecentMainBlock
  , getTipHeader
  , getTipSlotId
  , getSecurityParameter
  , getMaxTxSize
    -- * Support for tests
  , NodeStateUnavailable(..)
  , nodeStateUnavailable
  ) where

import           Universum

import           Control.Lens (lens)
import           Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (UnliftIO),
                     askUnliftIO, unliftIO, withUnliftIO)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Chain.Block (Block, BlockHeader, HeaderHash, MainBlock,
                     blockHeader, headerHash, mainBlockSlot, prevBlockL)
import           Pos.Chain.Update (HasUpdateConfiguration, bvdMaxTxSize)
import           Pos.Context (NodeContext (..))
import           Pos.Core (ProtocolConstants (pcK))
import           Pos.Core.Configuration (HasConfiguration, HasProtocolConstants,
                     genesisHash, protocolConstants)
import           Pos.Core.Slotting (EpochIndex (..), HasSlottingVar (..),
                     LocalSlotIndex (..), MonadSlots (..), SlotId (..))
import qualified Pos.DB.Block as DB
import qualified Pos.DB.BlockIndex as Core
import           Pos.DB.Class (MonadDBRead (..), getBlock)
import           Pos.DB.GState.Lock (StateLock, withStateLockNoMetrics)
import           Pos.DB.Rocks.Functions (dbGetDefault, dbIterSourceDefault)
import           Pos.DB.Rocks.Types (NodeDBs)
import           Pos.DB.Update (getAdoptedBVData)
import qualified Pos.Infra.Slotting.Impl.Simple as S
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Util (HasLens (..), lensOf')
import           Pos.Util.Concurrent.PriorityLock (Priority (..))

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
      HasConfiguration
    , HasUpdateConfiguration
    , HasProtocolConstants
    )

-- | Internal: node resources and reified type class dictionaries
data Res = forall ext. Res (NodeResources ext)

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

-- | The 'NodeStateAdaptor' allows to bring the node state into scope
-- without polluting all the types in the kernel.
--
-- See 'newNodeStateAdaptor'.
newtype NodeStateAdaptor m = Adaptor {
      withNodeState :: forall a.
                       (    NodeConstraints
                         => Lock (WithNodeState m)
                         -> WithNodeState m a
                       )
                    -> m a
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

instance HasSlottingVar Res where
    slottingTimestamp = mkResLens (nrContextLens . slottingTimestamp)
    slottingVar       = mkResLens (nrContextLens . slottingVar)

{-------------------------------------------------------------------------------
  Monad instances

  NOTE: Although these instances require 'NodeConstraints', 'withNodeContext'
  will make sure that this is in scope.
-------------------------------------------------------------------------------}

instance MonadUnliftIO m => MonadUnliftIO (WithNodeState m) where
  askUnliftIO = Wrap $ withUnliftIO $ \u ->
                  pure $ UnliftIO (unliftIO u . unwrap)

instance ( NodeConstraints
         , MonadThrow m
         , MonadIO    m
         , MonadCatch m
         ) => MonadDBRead (WithNodeState m) where
  dbGet         = dbGetDefault
  dbIterSource  = dbIterSourceDefault
  dbGetSerBlock = DB.dbGetSerBlockRealDefault
  dbGetSerUndo  = DB.dbGetSerUndoRealDefault

instance (NodeConstraints, MonadIO m) => MonadSlots Res (WithNodeState m) where
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
newNodeStateAdaptor :: (NodeConstraints, MonadIO m, MonadMask m)
                    => NodeResources ext -> NodeStateAdaptor m
newNodeStateAdaptor nr = Adaptor $ \act ->
    runReaderT (unwrap $ act withLock) (Res nr)

-- | Internal wrapper around 'withStateLockNoMetrics'
--
-- NOTE: If we wanted to use 'withStateLock' instead we would need to
-- capture additional node context.
withLock :: (NodeConstraints, MonadIO m, MonadMask m) => Lock (WithNodeState m)
withLock AlreadyLocked f = headerHash <$> Core.getTipHeader >>= f
withLock NotYetLocked  f = Wrap $ withStateLockNoMetrics LowPriority
                                $ unwrap . f

{-------------------------------------------------------------------------------
  Specific queries
-------------------------------------------------------------------------------}

-- | Get the most recent main block starting at the specified header
--
-- Returns nothing if there are no (regular) blocks on the blockchain yet.
mostRecentMainBlock :: forall m. (MonadIO m, MonadCatch m)
                    => NodeStateAdaptor m
                    -> HeaderHash
                    -> m (Maybe MainBlock)
mostRecentMainBlock node = \hdrHash -> withNodeState node $ \_lock ->
    go hdrHash
  where
    go :: NodeConstraints
       => HeaderHash
       -> WithNodeState m (Maybe MainBlock)
    go hdrHash
      | hdrHash == genesisHash = return Nothing
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

    getBlockOrThrow :: NodeConstraints => HeaderHash -> WithNodeState m Block
    getBlockOrThrow hdrHash = do
        mBlock <- getBlock hdrHash
        case mBlock of
          Nothing    -> throwM $ MissingBlock callStack hdrHash
          Just block -> return block

-- | Get the header of the tip of the chain
getTipHeader :: (MonadIO m, MonadCatch m) => NodeStateAdaptor m -> m BlockHeader
getTipHeader node = withNodeState node $ \_lock -> Core.getTipHeader

-- | Get the slot ID of the chain tip
--
-- Returns slot 0 in epoch 0 if there are no blocks yet.
getTipSlotId :: (MonadIO m, MonadCatch m) => NodeStateAdaptor m -> m SlotId
getTipSlotId node = do
    hdrHash <- headerHash <$> getTipHeader node
    aux <$> mostRecentMainBlock node hdrHash
  where
    aux :: Maybe MainBlock -> SlotId
    aux (Just mainBlock) = mainBlock ^. mainBlockSlot
    aux Nothing          = SlotId (EpochIndex 0) (UnsafeLocalSlotIndex 0)

-- | Get the security parameter (@k@)
getSecurityParameter :: Monad m => NodeStateAdaptor m -> m Int
getSecurityParameter node = withNodeState node $ \_lock ->
    return $ pcK protocolConstants

-- | Get maximum transaction size
getMaxTxSize :: (MonadIO m, MonadCatch m) => NodeStateAdaptor m -> m Byte
getMaxTxSize node =
    fmap bvdMaxTxSize $ withNodeState node $ \_lock -> getAdoptedBVData

-- | Thrown if we cannot find a previous block
--
-- If this ever happens it indicates a serious problem: the blockchain as
-- stored in the node is not correct.
data MissingBlock = MissingBlock CallStack HeaderHash
  deriving (Show)

instance Exception MissingBlock

{-------------------------------------------------------------------------------
  Support for tests
-------------------------------------------------------------------------------}

-- | Thrown when using the 'nodeStateUnavailable' adaptor.
data NodeStateUnavailable = NodeStateUnavailable
  deriving (Show)

instance Exception NodeStateUnavailable

-- | Node state adaptor for use in tests that throws an exception when used
nodeStateUnavailable :: MonadThrow m => NodeStateAdaptor m
nodeStateUnavailable = Adaptor $ \_act -> throwM NodeStateUnavailable
