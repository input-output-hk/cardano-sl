module Cardano.Wallet.Kernel.ChainState (
    -- * Chain state and state modifier
    ChainState(..)
  , ChainStateModifier(..)
  , fromCPS
  , applyChainStateModifier
    -- * Chain brief
  , ChainBrief(..)
  , getChainBrief
  , chainBriefSucceeds
    -- * Restoration
  , ChainStateRestoration(..)
  , getChainStateRestoration
  ) where

import           Universum

import qualified Data.Map.Strict as Map

import           Pos.Chain.Block (HeaderHash, gbHeader, headerHash,
                     mainBlockSlot, prevBlockL)
import           Pos.Chain.Update (BlockVersionData (..),
                     ConfirmedProposalState (..), HasUpdateConfiguration,
                     genesisBlockVersion, genesisSoftwareVersions, ourAppName)
import           Pos.Core (HasConfiguration, ScriptVersion, SlotId (..))
import           Pos.Core.Configuration (genesisBlockVersionData, genesisHash)
import           Pos.Core.Update (ApplicationName (..), BlockVersion (..),
                     BlockVersionModifier (..), NumSoftwareVersion,
                     SoftwareVersion (..), UpdateProposal (..))
import           Pos.DB.Update (getAdoptedBVFull, getConfirmedProposals,
                     getConfirmedSV)

import           Formatting (bprint, build, shown, (%))
import qualified Formatting.Buildable
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util (mapJson)

import           Cardano.Wallet.Kernel.NodeStateAdaptor (LockContext,
                     NodeConstraints, NodeStateAdaptor, WithNodeState,
                     mostRecentMainBlock, withNodeState)

{-------------------------------------------------------------------------------
  Chain state and state modifiers
-------------------------------------------------------------------------------}

-- | Chain state
--
-- This is an extract from a full chain state, containing only the variables
-- that the wallet is interested in.
data ChainState = ChainState {
      csBlockVersion    :: !BlockVersion
    , csSoftwareVersion :: !SoftwareVersion
    , csScriptVersion   :: !ScriptVersion
    , csMaxTxSize       :: !Byte
    }

-- | Chain state modifier
data ChainStateModifier = ChainStateModifier {
      csmBlockVersion    :: !BlockVersion
    , csmSoftwareVersion :: !SoftwareVersion
    , csmScriptVersion   :: !(Maybe ScriptVersion)
    , csmMaxTxSize       :: !(Maybe Byte)
    }

-- | The header of the block the proposal got confirmed in and the corresponding
-- 'ChainStateModifier'
fromCPS :: ConfirmedProposalState -> (HeaderHash, ChainStateModifier)
fromCPS ConfirmedProposalState{..} = (cpsConfirmed, ChainStateModifier {
      csmBlockVersion    = upBlockVersion
    , csmSoftwareVersion = upSoftwareVersion
    , csmScriptVersion   = bvmScriptVersion
    , csmMaxTxSize       = bvmMaxTxSize
    })
  where
    UnsafeUpdateProposal{..} = cpsUpdateProposal
    BlockVersionModifier{..} = upBlockVersionMod

-- | Apply a chain state modifier to a chain state
applyChainStateModifier :: ChainStateModifier -> ChainState -> ChainState
applyChainStateModifier ChainStateModifier{..} ChainState{..} = ChainState{
      csBlockVersion    =                           csmBlockVersion
    , csSoftwareVersion =                           csmSoftwareVersion
    , csScriptVersion   = fromMaybe csScriptVersion csmScriptVersion
    , csMaxTxSize       = fromMaybe csMaxTxSize     csmMaxTxSize
    }

{-------------------------------------------------------------------------------
  Chain summary
-------------------------------------------------------------------------------}

-- | Self-consistent summary of the current tip of the chain
data ChainBrief = ChainBrief {
      -- | Header hash of the chain tip, according to the node DB
      cbTip      :: !HeaderHash

      -- | Slot ID of the tip
      --
      -- NOTE: This is /not/ the "current" slot ID, but rather the slot
      -- associated with the 'cbTip'. The intention is that 'ChainBrief'
      -- provides a consistent view of the node's database for restoration
      -- purposes. If the current slot ID (based on timestamp) is needed, use
      -- the 'MonadSlots' interface (but I strongly suspect 'MonadSlots' may in
      -- fact only be needed by the underlying node).
    , cbSlotId   :: !SlotId

      -- | Header of the previous main block
      --
      -- If there is no previous main block, this is set to the genesis hash.
    , cbPrevMain :: !HeaderHash

      -- | The chain state at the time of 'cbTip'
      --
      -- Implementation note: Although we have no way of verifying that these
      -- actually match up, the hope is that since we read all these values
      -- while locking the node state, that they will be consistent with each
      -- other. That might be overly optimistic.
    , cbState    :: !ChainState
    }

-- | Get 'ChainBrief' for current chain tip
--
-- Returns
--
-- * 'Nothing' if the current tip is the genesis block
-- * The previous block if the current tip is an epoch boundary block (EBB)
-- * The current block if the current tip is a regular block.
getChainBrief :: HasCallStack
              => NodeStateAdaptor IO
              -> LockContext
              -> IO (Maybe ChainBrief)
getChainBrief node lc = withNodeState node $ \withLock -> do
    (tip, (bv, bvd), sv) <- withLock lc $ \tip ->
            (tip,,)
        <$> getAdoptedBVFull
        <*> getVersionOrThrow

    mMainBlock <- mostRecentMainBlock tip
    case mMainBlock of
      Nothing ->
        return Nothing
      Just mainBlock -> do
        let slotId = mainBlock ^. mainBlockSlot
            hdr    = mainBlock ^. gbHeader
        mPrevMain <- mostRecentMainBlock (hdr ^. prevBlockL)
        return $ Just ChainBrief {
            cbSlotId   = slotId
          , cbTip      = headerHash hdr
          , cbPrevMain = case mPrevMain of
                           Just prevMain -> headerHash prevMain
                           Nothing       -> genesisHash
          , cbState    = ChainState {
                csBlockVersion    = bv
              , csScriptVersion   = bvdScriptVersion bvd
              , csMaxTxSize       = bvdMaxTxSize     bvd
              , csSoftwareVersion = sv
              }
          }
  where
    getVersionOrThrow :: NodeConstraints => WithNodeState IO SoftwareVersion
    getVersionOrThrow = do
        mSV <- getConfirmedSV ourAppName
        case mSV of
          Nothing -> throwM $ ChainStateMissingVersion callStack ourAppName
          Just sv -> return $ SoftwareVersion ourAppName sv

-- | @a `chainBriefSucceeds` b@ is true when @a@ is the successor of @b@.
chainBriefSucceeds :: ChainBrief -> ChainBrief -> Bool
a `chainBriefSucceeds` b = cbPrevMain a == cbTip b

{-------------------------------------------------------------------------------
  Restoration
-------------------------------------------------------------------------------}

data ChainStateRestoration = ChainStateRestoration {
      -- | Initial chain state
      --
      -- This provides a base case for applying the 'ChainStateModifier's
      csrGenesis :: !ChainState

      -- | All updates, indexed by the block in which they were confirmed
    , csrUpdates :: !(Map HeaderHash ChainStateModifier)

      -- | Current chain state
      --
      -- This provides the target for restoration, as well as its starting
      -- point: we synchronously create a checkpoint for the current tip, and
      -- then asynchronously restore the missing checkpoints (possibly from
      -- genesis when we are restoring, or from another checkpoint if we are
      -- catching up).
      --
      -- If this field is 'Nothing', the chain does not contain any main blocks.
    , csrCurrent :: !(Maybe ChainBrief)
    }

-- | Get all information needed for restoration
getChainStateRestoration :: HasCallStack
                         => NodeStateAdaptor IO
                         -> LockContext
                         -> IO ChainStateRestoration
getChainStateRestoration node lc = do
    current <- getChainBrief node lc
    -- We don't need to lock now -- it's possible (in principle) that there
    -- might be a new proposal at this point, but old proposals should still
    -- exist.
    --
    -- TODO: Unless this removes proposals that get rolled back...?
    withNodeState node $ \_lock -> do
      proposals <- getConfirmedProposals allVersions
      sv <- case genesisSoftwareVersion of
              Just sv -> return sv
              Nothing -> throwM $ ChainStateMissingVersion callStack ourAppName
      return ChainStateRestoration{
            csrGenesis = initChainState sv
          , csrUpdates = Map.fromList $ map fromCPS proposals
          , csrCurrent = current
          }
  where
    -- We want all updates across all versions
    allVersions :: Maybe NumSoftwareVersion
    allVersions = Nothing

    -- Initial chain state (at the start of the blockchain)
    --
    -- At the moment this gets determined by the configuration.yaml file
    initChainState :: HasConfiguration => SoftwareVersion -> ChainState
    initChainState sv = ChainState{
          csBlockVersion    = genesisBlockVersion
        , csSoftwareVersion = sv
        , csScriptVersion   = bvdScriptVersion
        , csMaxTxSize       = bvdMaxTxSize
        }
      where
        BlockVersionData{..} = genesisBlockVersionData

    genesisSoftwareVersion :: HasUpdateConfiguration => Maybe SoftwareVersion
    genesisSoftwareVersion = listToMaybe $ filter isOurs genesisSoftwareVersions

    isOurs :: HasUpdateConfiguration => SoftwareVersion -> Bool
    isOurs sv = svAppName sv == ourAppName

{-------------------------------------------------------------------------------
  Custom exceptions
-------------------------------------------------------------------------------}

data ChainStateException =
    ChainStateMissingVersion CallStack ApplicationName
  deriving (Show)

instance Exception ChainStateException

{-------------------------------------------------------------------------------
  Pretty printing
-------------------------------------------------------------------------------}

instance Buildable ChainState where
  build ChainState{..} = bprint
    ( "ChainState "
    % "{ blockVersion:    " % build
    % ", softwareVersion: " % build
    % ", scriptVersion:   " % build
    % ", maxTxSize:       " % shown
    % "}"
    )
    csBlockVersion
    csSoftwareVersion
    csScriptVersion
    csMaxTxSize

instance Buildable ChainStateModifier where
  build ChainStateModifier{..} = bprint
    ( "ChainStateModifier "
    % "{ blockVersion:    " % build
    % ", softwareVersion: " % build
    % ", scriptVersion:   " % build
    % ", maxTxSize:       " % shown
    % "}"
    )
    csmBlockVersion
    csmSoftwareVersion
    csmScriptVersion
    csmMaxTxSize

instance Buildable ChainStateRestoration where
  build ChainStateRestoration{..} = bprint
    ( "ChainStateRestoration "
    % "{ genesis: " % build
    % ", updates: " % mapJson
    % ", current: " % build
    % "}"
    )
    csrGenesis
    csrUpdates
    csrCurrent

instance Buildable ChainBrief where
  build ChainBrief{..} = bprint
    ( "ChainBrief "
    % ", slotId: " % build
    % ", tip:    " % build
    % ", state:  " % build
    % "}"
    )
    cbSlotId
    cbTip
    cbState
