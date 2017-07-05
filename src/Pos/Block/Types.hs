-- | Types used for block processing.  I suppose this module is
-- temporary, I expect us to have a meeting in near future.

module Pos.Block.Types
       ( SlogUndo (..)
       , Undo (..)
       , Blund
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting            (bprint, build, (%))
import           Serokell.Util.Text    (listJson)

import           Pos.Block.Core        (BiSsc, Block)
import           Pos.Core              (FlatSlotId, HasDifficulty (..),
                                        HasHeaderHash (..), slotIdF, unflattenSlotId)
import           Pos.Delegation.Types  (DlgUndo)
import           Pos.Txp.Core          (TxpUndo)
import           Pos.Update.Poll.Types (USUndo)

-- | Undo data from Slog, i. e. data which is necessary do rollback a
-- block inside Slog.
--
-- If block is one of the first 'blkSecurityParam' blocks, we don't
-- need to store anything. We also don't need to store anything for
-- genesis blocks. Otherwise we store 'FlatSlotId' of the oldest block
-- from those for which we stored slots before given block was
-- applied.
newtype SlogUndo = SlogUndo
    { getSlogUndo :: Maybe FlatSlotId
    } deriving (NFData)

instance Buildable SlogUndo where
    build (SlogUndo oldSlot) =
        "SlogUndo: " <>
        maybe "<nothing>" (bprint slotIdF . unflattenSlotId) oldSlot

-- | Structure for undo block during rollback
data Undo = Undo
    { undoTx   :: !TxpUndo
    , undoPsk  :: !DlgUndo
    , undoUS   :: !USUndo
    , undoSlog :: !SlogUndo
    } deriving (Generic)

instance NFData Undo

-- instance Default Undo where
--     def = Undo {undoTx = mempty, undoPsk = mempty, undoUS = def}

-- | Block and its Undo.
type Blund ssc = (Block ssc, Undo)

instance Buildable Undo where
    build Undo{..} =
        bprint ("Undo:\n"%
                "  undoTx: "%listJson%"\n"%
                "  undoPsk: "%listJson%"\n"%
                "  undoUS: "%build%
                "  undoSlog: "%build)
               (map (bprint listJson) undoTx) undoPsk undoUS undoSlog

instance HasDifficulty (Blund ssc) where
    difficultyL = _1 . difficultyL

instance BiSsc ssc => HasHeaderHash (Blund ssc) where
    headerHash = headerHash . fst
