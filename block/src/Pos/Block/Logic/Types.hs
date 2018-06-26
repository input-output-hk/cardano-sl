module Pos.Block.Logic.Types
    ( VerifyBlocksContext (..)
    , getVerifyBlocksContext
    , getVerifyBlocksContext'
    ) where

import           Universum

import           Pos.Core (BlockVersion, BlockVersionData)
import           Pos.Core.Slotting (SlotId)
import           Pos.DB.Class (MonadDBRead)
import           Pos.Infra.Slotting (MonadSlots (getCurrentSlot))
import           Pos.Update.DB (getAdoptedBVFull)

-- | Initial context for `verifyBlocksPrefix` which runs in `MonadBlockVerify`
-- monad.
data VerifyBlocksContext = VerifyBlocksContext
    { vbcCurrentSlot      :: !(Maybe SlotId)
      -- ^ used to check if headers are not from future
    , vbcBlockVersion     :: !BlockVersion
    , vbcBlockVersionData :: !BlockVersionData
    } deriving Generic

instance NFData VerifyBlocksContext

getVerifyBlocksContext
    :: forall ctx m.
       ( MonadDBRead m
       , MonadSlots ctx m
       )
    => m VerifyBlocksContext
getVerifyBlocksContext =
    getCurrentSlot >>= getVerifyBlocksContext'

getVerifyBlocksContext'
    :: MonadDBRead m
    => Maybe SlotId
    -> m VerifyBlocksContext
getVerifyBlocksContext' vbcCurrentSlot = do
    (vbcBlockVersion, vbcBlockVersionData) <- getAdoptedBVFull
    return $ VerifyBlocksContext {..}
