-- | Types used for block processing.  I suppose this module is
-- temporary, I expect us to have a meeting in near future.

module Pos.Block.Types
       ( Undo (..)
       , Blund
       ) where

import           Data.Default          (Default (def))
import qualified Data.Text.Buildable
import           Formatting            (bprint, build, (%))
import           Serokell.Util.Text    (listJson)
import           Universum

import           Pos.Types.Block       (BiSsc, Block)
import           Pos.Types.Core        (HasDifficulty (..), HasHeaderHash (..))
import           Pos.Types.Types       (ProxySKSimple, TxUndo)
import           Pos.Update.Poll.Types (USUndo)

-- | Structure for undo block during rollback
data Undo = Undo
    { undoTx  :: !TxUndo
    , undoPsk :: ![ProxySKSimple] -- ^ PSKs we've overwritten/deleted
    , undoUS  :: !USUndo
    }

instance Default Undo where
    def = Undo {undoTx = mempty, undoPsk = mempty, undoUS = def}

-- | Block and its Undo.
type Blund ssc = (Block ssc, Undo)

instance Buildable Undo where
    build Undo{..} =
        bprint ("Undo:\n"%
                "  undoTx: "%listJson%"\n"%
                "  undoPsk: "%listJson%"\n"%
                "  undoUS: "%build)
               (map (bprint listJson) undoTx) undoPsk undoUS

instance HasDifficulty (Blund ssc) where
    difficultyL = _1 . difficultyL

instance BiSsc ssc => HasHeaderHash (Blund ssc) where
    headerHash = headerHash . fst
