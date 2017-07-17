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
import           Pos.Block.Slog.Types  (SlogUndo (..))
import           Pos.Core              (HasDifficulty (..), HasHeaderHash (..))
import           Pos.Delegation.Types  (DlgUndo)
import           Pos.Txp.Core          (TxpUndo)
import           Pos.Update.Poll.Types (USUndo)

-- | Structure for undo block during rollback
data Undo = Undo
    { undoTx   :: !TxpUndo
    , undoPsk  :: !DlgUndo
    , undoUS   :: !USUndo
    , undoSlog :: !SlogUndo
    } deriving (Generic)

instance NFData Undo

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
