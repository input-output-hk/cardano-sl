-- | Types used for block processing.  I suppose this module is
-- temporary, I expect us to have a meeting in near future.

module Pos.Block.Types
       ( Undo (..)
       , Blund
       ) where

import           Universum

import           Data.Default          (Default (def))
import qualified Data.Text.Buildable
import           Formatting            (bprint, build, (%))
import           Serokell.Util.Text    (listJson)

import           Pos.Block.Core        (BiSsc, Block)
import           Pos.Core              (HasDifficulty (..), HasHeaderHash (..),
                                        ProxySKHeavy)
import           Pos.Txp.Core          (TxpUndo)
import           Pos.Update.Poll.Types (USUndo)

-- | Structure for undo block during rollback
data Undo = Undo
    { undoTx  :: !TxpUndo
    , undoPsk :: ![ProxySKHeavy] -- ^ PSKs we've overwritten/deleted
    , undoUS  :: !USUndo
    } deriving (Generic)

instance NFData Undo

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
