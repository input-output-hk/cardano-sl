{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Cardano.Wallet.Kernel.NodeStateAdaptor (
    WithNodeState     -- opaque
  , NodeStateAdaptor  -- opaque
  , withNodeState
  , newNodeStateAdaptor
  , NodeConstraints
    -- * Additional types
  , SecurityParameter(..)
  , UnknownEpoch(..)
  , MissingBlock(..)
    -- * Locking
  , Lock
  , LockContext(..)
    -- * Specific queries
  , mostRecentMainBlock
  , getTipSlotId
  , getSecurityParameter
  , getMaxTxSize
  , getSlotCount
  , getSlotStart
  , getNextEpochSlotDuration
  , curSoftwareVersion
  , compileInfo
  , getNtpStatus
    -- * Support for tests
  , NodeStateUnavailable(..)
  , MockNodeStateParams(..)
  , mockNodeState
  , mockNodeStateDef
  , defMockNodeStateParams
  ) where

import           Universum

import           Control.Lens (lens)
import           Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (UnliftIO),
                     askUnliftIO, unliftIO, withUnliftIO)
import           Data.SafeCopy (base, deriveSafeCopy)
import           Data.Time.Units (Millisecond)
import           Formatting (bprint, build, sformat, shown, (%))
import qualified Formatting.Buildable
import           Ntp.Client (NtpStatus (..))
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Chain.Block (Block, HeaderHash, MainBlock, blockHeader,
                     headerHash, mainBlockSlot, prevBlockL)
import           Pos.Chain.Update (HasUpdateConfiguration, SoftwareVersion,
                     bvdMaxTxSize)
import qualified Pos.Chain.Update as Upd
import           Pos.Context (NodeContext (..))
import           Pos.Core (ProtocolConstants (pcK), SlotCount, Timestamp,
                     genesisBlockVersionData, pcEpochSlots)
import           Pos.Core.Configuration (HasConfiguration, genesisHash,
                     protocolConstants)
import           Pos.Core.Slotting (EpochIndex (..), HasSlottingVar (..),
                     LocalSlotIndex (..), MonadSlots (..), SlotId (..))
import qualified Pos.DB.Block as DB
import           Pos.DB.BlockIndex (getTipHeader)
import           Pos.DB.Class (MonadDBRead (..), getBlock)
import           Pos.DB.GState.Lock (StateLock, withStateLockNoMetrics)
import           Pos.DB.Rocks.Functions (dbGetDefault, dbIterSourceDefault)
import           Pos.DB.Rocks.Types (NodeDBs)
import           Pos.DB.Update (getAdoptedBVData)
import qualified Pos.Infra.Slotting.Impl.Simple as S
import qualified Pos.Infra.Slotting.Util as Slotting
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Util (CompileTimeInfo, HasCompileInfo, HasLens (..),
                     lensOf', withCompileInfo)
import qualified Pos.Util as Util
import           Pos.Util.Concurrent.PriorityLock (Priority (..))

import           Test.Pos.Configuration (withDefConfiguration,
                     withDefUpdateConfiguration)

{-------------------------------------------------------------------------------
  Additional types
-------------------------------------------------------------------------------}

newtype SecurityParameter = SecurityParameter Int

deriveSafeCopy 1 'base ''SecurityParameter

pcK' :: ProtocolConstants -> SecurityParameter
pcK' = SecurityParameter . pcK

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
      HasConfiguration
    , HasUpdateConfiguration
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

      -- | Get the start of a slot
      --
      -- When looking up data for past of the current epochs, the value should
      -- be known.
    , getSlotStart :: SlotId -> m (Either UnknownEpoch Timestamp)

      -- | Get last known slot duration.
    , getNextEpochSlotDuration :: m Millisecond

      -- | Version of application (code running)
    , curSoftwareVersion :: m SoftwareVersion

      -- | Git revision
    , compileInfo :: m CompileTimeInfo

      -- | Ask the NTP client for the status
    , getNtpStatus :: m NtpStatus
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
  dbGetSerBlund  = DB.dbGetSerBlundRealDefault

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
newNodeStateAdaptor :: forall m ext. (NodeConstraints, MonadIO m, MonadMask m)
                    => NodeResources ext
                    -> TVar NtpStatus
                    -> NodeStateAdaptor m
newNodeStateAdaptor nr ntpStatus = Adaptor {
      withNodeState            =            run
    , getTipSlotId             =            run $ \_lock -> defaultGetTipSlotId
    , getMaxTxSize             =            run $ \_lock -> defaultGetMaxTxSize
    , getSlotStart             = \slotId -> run $ \_lock -> defaultGetSlotStart slotId
    , getNextEpochSlotDuration =            run $ \_lock -> defaultGetNextEpochSlotDuration
    , getSecurityParameter     = return $ pcK'         protocolConstants
    , getSlotCount             = return $ pcEpochSlots protocolConstants
    , curSoftwareVersion       = return $ Upd.curSoftwareVersion
    , compileInfo              = return $ Util.compileInfo
    , getNtpStatus             = liftIO $ readTVarIO ntpStatus
    }
  where
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

-- | Get the slot ID of the chain tip
--
-- Returns slot 0 in epoch 0 if there are no blocks yet.
defaultGetTipSlotId :: (MonadIO m, MonadCatch m, NodeConstraints)
                    => WithNodeState m SlotId
defaultGetTipSlotId = do
    hdrHash <- headerHash <$> getTipHeader
    aux <$> mostRecentMainBlock hdrHash
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

-- | Get the most recent main block starting at the specified header
--
-- Returns nothing if there are no (regular) blocks on the blockchain yet.
mostRecentMainBlock :: forall m. (MonadIO m, MonadCatch m, NodeConstraints)
                    => HeaderHash -> WithNodeState m (Maybe MainBlock)
mostRecentMainBlock = go
  where
    go :: HeaderHash -> WithNodeState m (Maybe MainBlock)
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

    getBlockOrThrow :: HeaderHash -> WithNodeState m Block
    getBlockOrThrow hdrHash = do
        mBlock <- getBlock hdrHash
        case mBlock of
          Nothing    -> throwM $ MissingBlock callStack hdrHash
          Just block -> return block

{-------------------------------------------------------------------------------
  Support for tests
-------------------------------------------------------------------------------}

-- | Thrown when using the 'nodeStateUnavailable' adaptor.
data NodeStateUnavailable = NodeStateUnavailable CallStack
  deriving (Show)

instance Exception NodeStateUnavailable

-- | Node state adaptor for use in tests
--
-- See 'NodeStateAdaptor' for an explanation about what is and what is not
-- mockable.
mockNodeState :: (HasCallStack, MonadThrow m)
              => MockNodeStateParams -> NodeStateAdaptor m
mockNodeState MockNodeStateParams{..} =
    withDefConfiguration $ \_pm ->
    withDefUpdateConfiguration $
      Adaptor {
          withNodeState            = \_ -> throwM $ NodeStateUnavailable callStack
        , getTipSlotId             = return mockNodeStateTipSlotId
        , getSecurityParameter     = return mockNodeStateSecurityParameter
        , getNextEpochSlotDuration = return mockNodeStateNextEpochSlotDuration
        , getNtpStatus             = return mockNodeStateNtpStatus
        , getSlotStart             = return . mockNodeStateSlotStart
        , getMaxTxSize             = return $ bvdMaxTxSize genesisBlockVersionData
        , getSlotCount             = return $ pcEpochSlots protocolConstants
        , curSoftwareVersion       = return $ Upd.curSoftwareVersion
        , compileInfo              = return $ Util.compileInfo
        }

-- | Variation on 'mockNodeState' that uses the default params
mockNodeStateDef :: (HasCallStack, MonadThrow m) => NodeStateAdaptor m
mockNodeStateDef = mockNodeState defMockNodeStateParams

-- | Parameters for 'mockNodeState'
--
-- NOTE: These values are intentionally not strict, so that we can provide
-- error values in 'defMockNodeStateParams'
data MockNodeStateParams = NodeConstraints => MockNodeStateParams {
        -- | Value for 'getTipSlotId'
        mockNodeStateTipSlotId :: SlotId

        -- | Value for 'getSlotSTart'
      , mockNodeStateSlotStart :: SlotId -> Either UnknownEpoch Timestamp

        -- | Value for 'getSecurityParameter'
      , mockNodeStateSecurityParameter :: SecurityParameter

        -- | Value for 'getNextEpochSlotDuration'
      , mockNodeStateNextEpochSlotDuration :: Millisecond

        -- | Value for 'getNtpStatus'
      , mockNodeStateNtpStatus :: NtpStatus
      }

-- | Default 'MockNodeStateParams'
--
-- NOTE:
--
-- * Most of the default parameters are error values
-- * The 'NodeConstraints' that come from the test configuration
-- * However, we set the security parameter to 2160 instead of taking that
--   from the test configuration, since in the test configuration @k@ is
--   assigned a really low value, which would cause us to throw away from
--   checkpoints during testing that we should not throw away.
defMockNodeStateParams :: MockNodeStateParams
defMockNodeStateParams =
    withDefConfiguration $ \_pm ->
    withDefUpdateConfiguration $
    withCompileInfo $
      MockNodeStateParams {
          mockNodeStateTipSlotId             = notDefined "mockNodeStateTipSlotId"
        , mockNodeStateSlotStart             = notDefined "mockNodeStateSlotStart"
        , mockNodeStateNextEpochSlotDuration = notDefined "mockNodeStateNextEpochSlotDuration"
        , mockNodeStateSecurityParameter     = SecurityParameter 2160
        , mockNodeStateNtpStatus             = NtpSyncUnavailable
        }
  where
    notDefined :: Text -> a
    notDefined = error
              . sformat ("defMockNodeStateParams: '" % build % "' not defined")

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable MissingBlock where
  build (MissingBlock cs hash) =
    bprint ("MissingBlock " % build % " at " % shown) hash (prettyCallStack cs)

instance Buildable UnknownEpoch where
  build (UnknownEpoch slotId) =
    bprint ("UnknownEpoch " % build) slotId
