module Cardano.Wallet.Kernel.DB.BlockContext (
    -- * Block context
    BlockContext(..)
  , blockContextSucceeds
    -- ** Lenses
  , bcSlotId
  , bcHash
  , bcPrevMain
    -- * Construction
  , mainBlockContext
  ) where

import           Universum

import qualified Cardano.Wallet.Kernel.Util.Strict as Strict
import           Control.Lens (lazy, strict)

import           Control.Lens (makeLenses)
import           Data.SafeCopy (base, deriveSafeCopy)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable

import qualified Pos.Chain.Block as Core
import           Pos.Chain.Genesis (GenesisHash)
import qualified Pos.Core as Core

import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.NodeStateAdaptor

{-------------------------------------------------------------------------------
  Block context
-------------------------------------------------------------------------------}

-- | Information about where a block is placed in the chain
data BlockContext = BlockContext {
      -- | Slot ID of this block
      _bcSlotId   :: !(InDb Core.SlotId)

      -- | Header hash of this block
    , _bcHash     :: !(InDb Core.HeaderHash)

      -- | Header hash of the previous /main/ block
      --
      -- NOTE: Since this is used in 'applyBlock' to check whether or not
      -- this block fits onto the chain, and we only apply main blocks,
      -- it is important that if the raw block's previous pointer to an EBB,
      -- we do some work to figure out what the previous /main/ block was.
      -- See 'mostRecentMainBlock'.
    , _bcPrevMain :: !(Strict.Maybe (InDb Core.HeaderHash))
    } deriving Eq

makeLenses ''BlockContext
deriveSafeCopy 1 'base ''BlockContext

-- | Check if one checkpoint succeeds another
--
-- The second argument is a 'Maybe', because the first checkpoint in an account
-- will have no context. The first argument is /not/ a 'Maybe' because /ONLY/
-- the first checkpoint in an account can have no context.
blockContextSucceeds :: BlockContext -> Maybe BlockContext -> Bool
_ `blockContextSucceeds` Nothing  = True
a `blockContextSucceeds` (Just b) =
    case a ^. bcPrevMain . lazy of
      Nothing   -> False -- Previous checkpoint must have been the initial one
      Just prev -> prev == b ^. bcHash

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

mainBlockContext :: (NodeConstraints, MonadIO m, MonadCatch m)
                 => GenesisHash -> Core.MainBlock -> WithNodeState m BlockContext
mainBlockContext genesisHash mb = do
    mPrev <- view strict <$>
        mostRecentMainBlock genesisHash (mb ^. Core.mainBlockPrevBlock)
    return BlockContext {
        _bcSlotId   = InDb $ mb ^. Core.mainBlockSlot
      , _bcHash     = InDb $ Core.headerHash mb
      , _bcPrevMain = (InDb . Core.headerHash) <$> mPrev
      }

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable BlockContext where
    build BlockContext{..} = bprint
      ( "BlockContext "
      % "{ slotId " % build
      % ", hash   " % build
      % ", prev   " % build
      % "}"
      )
      _bcSlotId
      _bcHash
      _bcPrevMain
