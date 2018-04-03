-- | Types used for block processing: most importantly, 'Undo' and 'Blund'.

module Pos.Block.Types
       ( SlogUndo (..)
       , Undo (..)
       , Blund
       , SerializedBlund

       , LastKnownHeader
       , LastKnownHeaderTag
       , MonadLastKnownHeader

       , RecoveryHeaderTag
       , RecoveryHeader
       , MonadRecoveryHeader
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))
import           Serokell.Util.Text (listJson)

import           Pos.Block.Slog.Types (SlogUndo (..))
import           Pos.Communication.Protocol (NodeId)
import           Pos.Core (HasConfiguration, HasDifficulty (..), HasHeaderHash (..))
import           Pos.Core.Block (Block, BlockHeader)
import           Pos.Core.Txp (TxpUndo)
import           Pos.DB.Class (SerializedUndo)
import           Pos.Delegation.Types (DlgUndo)
import           Pos.Update.Poll.Types (USUndo)
import           Pos.Util.Util (HasLens (..))

-- | Structure for undo block during rollback
data Undo = Undo
    { undoTx   :: !TxpUndo
    , undoDlg  :: !DlgUndo
    , undoUS   :: !USUndo
    , undoSlog :: !SlogUndo
    } deriving (Eq, Show, Generic)

instance NFData Undo

-- | Block and its Undo.
type Blund = (Block, Undo)

type SerializedBlund = (Block, SerializedUndo)

instance HasConfiguration => Buildable Undo where
    build Undo{..} =
        bprint ("Undo:\n"%
                "  undoTx: "%listJson%"\n"%
                "  undoDlg: "%build%"\n"%
                "  undoUS: "%build%"\n"%
                "  undoSlog: "%build)
               (map (bprint listJson) undoTx) undoDlg undoUS undoSlog

instance HasDifficulty Blund where
    difficultyL = _1 . difficultyL

instance HasConfiguration => HasHeaderHash Blund where
    headerHash = headerHash . fst

-- | For a description of what these types mean,
-- please refer to @NodeContext@ in @Pos.Context.Context@.
data LastKnownHeaderTag
type LastKnownHeader = TVar (Maybe BlockHeader)
type MonadLastKnownHeader ctx m
     = (MonadReader ctx m, HasLens LastKnownHeaderTag ctx LastKnownHeader)

data RecoveryHeaderTag
type RecoveryHeader = STM.TMVar (NodeId, BlockHeader)
type MonadRecoveryHeader ctx m
     = (MonadReader ctx m, HasLens RecoveryHeaderTag ctx RecoveryHeader)
