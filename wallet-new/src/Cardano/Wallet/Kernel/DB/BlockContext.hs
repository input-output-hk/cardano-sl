module Cardano.Wallet.Kernel.DB.BlockContext (
    -- * Block context
    BlockContext(..)
  , blockContextSucceeds
    -- ** Lenses
  , bcSlotId
  , bcHash
  , bcHeight
  , bcPrevMain
  , bcPrevMainHeight
    -- * Construction
  , mainBlockContext
  ) where

import           Universum

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
      _bcSlotId         :: !(InDb Core.SlotId)

      -- | Header hash of this block
    , _bcHash           :: !(InDb Core.HeaderHash)

    -- | The height of this block. The height is generally just a number
    -- measuring how "far" we are from the genesis block. For example, the
    -- genesis block has an height of 0, the block immediately following it
    -- 1 etc etc.
    , _bcHeight         :: !(InDb Core.ChainDifficulty)

      -- | Header hash of the previous /main/ block
      --
      -- NOTE: Since this is used in 'applyBlock' to check whether or not
      -- this block fits onto the chain, and we only apply main blocks,
      -- it is important that if the raw block's previous pointer to an EBB,
      -- we do some work to figure out what the previous /main/ block was.
      -- See 'mostRecentMainBlock'.
    , _bcPrevMain       :: !(Maybe (InDb Core.HeaderHash))

      -- | Blockchain height of the previous /main/ block
    , _bcPrevMainHeight :: !(Maybe (InDb Core.ChainDifficulty))
    } deriving Eq

makeLenses ''BlockContext
deriveSafeCopy 1 'base ''BlockContext

-- | Check if one checkpoint succeeds another. One checkpoint succeeds another
-- if it has an bigger depth of the predecessor.
--
-- The second argument is a 'Maybe', because the first checkpoint in an account
-- will have no context. The first argument is /not/ a 'Maybe' because /ONLY/
-- the first checkpoint in an account can have no context.
blockContextSucceeds :: BlockContext -> Maybe BlockContext -> Bool
_ `blockContextSucceeds` Nothing  = True
a `blockContextSucceeds` (Just b) = a ^. bcHeight > b ^. bcHeight

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

mainBlockContext :: (NodeConstraints, MonadIO m, MonadCatch m)
                 => GenesisHash -> Core.MainBlock -> WithNodeState m BlockContext
mainBlockContext genesisHash mb = do
    mPrev <- mostRecentMainBlock genesisHash (mb ^. Core.mainBlockPrevBlock)
    return BlockContext {
        _bcSlotId         = InDb $ mb ^. Core.mainBlockSlot
      , _bcHash           = InDb $ Core.headerHash mb
      , _bcHeight         = InDb $ mb ^. Core.difficultyL
      , _bcPrevMain       = (InDb . Core.headerHash) <$> mPrev
      , _bcPrevMainHeight = (InDb . view Core.difficultyL) <$> mPrev
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
