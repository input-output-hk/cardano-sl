{-# LANGUAGE TypeFamilies           #-}

-- | Translation of an abstract chain into Cardano.
module Chain.Abstract.Translate.ToCardano where

import           Chain.Abstract (Addr, SlotId, BlockHash, StakeDistribution)
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Except
import           Universum
import           UTxO.Interpreter (Interpretation(..), Interpret(..))

{-------------------------------------------------------------------------------
  Translation context
-------------------------------------------------------------------------------}

-- | Checkpoint (we create one for each block we translate)
data IntCheckpoint = IntCheckpoint {
      -- | Slot number of this checkpoint
      icSlotId    :: !SlotId

      -- | Hash of the current block
    , icBlockHash :: !BlockHash

      -- | Running stakes
    , icStakes    :: !(StakeDistribution Addr)

      -- | Delegation graph. This is instantiated to the identify function.
    , icDlg       :: Addr -> Addr
    }

-- | Translation state
data TransState h = TransState {
      -- | Transaction map
      -- _tsTx          :: !(Map (h (DSL.Transaction h Addr)) (Transaction h Addr))

      -- | Checkpoints
      _tsCheckpoints :: !(NonEmpty IntCheckpoint)
    }

makeLenses ''TransState

-- | Translation context. This the read-only data available to translation.
data TransCtxt h = TransCtxt
  { -- | All actors in the system.
    _tcAddresses :: NonEmpty Addr
  }

makeLenses ''TransCtxt

{-------------------------------------------------------------------------------
Errors that may occur during interpretation
-------------------------------------------------------------------------------}

-- | Interpretation error
data IntException = IntException deriving Show -- TODO

instance Exception IntException

{-------------------------------------------------------------------------------
  The interpretation monad
-------------------------------------------------------------------------------}

-- | Interpretation monad
newtype TranslateT h e m a = TranslateT {
    -- untranslateT :: StateT (IntCtxt h) (TranslateT (Either IntException e) m) a  -- from UTxO.Interpreter
    unTranslateT :: StateT (TransState h) (ReaderT (TransCtxt h) (ExceptT e m)) a -- from Chain.Abstract.Translate.FromUTxO
    }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError e
             , MonadIO
             , MonadReader (TransCtxt h)
             , MonadState (TransState h)
             )

{-------------------------------------------------------------------------------
  Translate the Abstract chain definitions to Cardano types
-------------------------------------------------------------------------------}

data Abstract2Cardano

-- instance Interpretation Abstract2Cardano where
--   type IntCtx Abstract2Cardano = TranslateT
