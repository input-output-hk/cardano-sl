{-# LANGUAGE TypeFamilies           #-}

-- | Translation of an abstract chain into Cardano.
module Chain.Abstract.Translate.ToCardano where

import           Chain.Abstract (Addr, SlotId, BlockHash, StakeDistribution)
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Except
import           Universum
import           UTxO.Context (TransCtxt(..))
import           UTxO.Interpreter (Interpretation(..), Interpret(..))

{-------------------------------------------------------------------------------
  Translation context
-------------------------------------------------------------------------------}

-- | Checkpoint (we create one for each block we translate)
data IntCheckpoint

-- | Interpretation context
data IntCtxt (h :: * -> *)

makeLenses ''IntCtxt

-- | Translation state
data TransState (h :: * -> *)

makeLenses ''TransState

{-------------------------------------------------------------------------------
Errors that may occur during interpretation
-------------------------------------------------------------------------------}

-- | Interpretation error
data IntException = IntException deriving Show -- TODO

instance Exception IntException

{-------------------------------------------------------------------------------
  Translation monad

  The translation provides access to the translation context as well as some
  dictionaries so that we can lift Cardano operations to the 'Translate' monad.
  (Eventually we may wish to do this differently.)
-------------------------------------------------------------------------------}

-- | Translation environment
--
-- NOTE: As we reduce the scope of 'HasConfiguration' and
-- 'HasUpdateConfiguration', those values should be added into the
-- 'CardanoContext' instead.
data TranslateEnv = TranslateEnv {
      teContext :: TransCtxt
    }

newtype TranslateT e m a = TranslateT {
      unTranslateT :: ExceptT e (ReaderT TranslateEnv m) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError e
           , MonadIO
           , MonadFail
           )

instance MonadTrans (TranslateT e) where
  lift = TranslateT . lift . lift

type Translate e = TranslateT e Identity

instance Monad m => MonadReader TransCtxt (TranslateT e m) where
  ask     = TranslateT $ asks teContext
  local f = TranslateT . local f' . unTranslateT
    where
      f' env = env { teContext = f (teContext env) }

{-------------------------------------------------------------------------------
  The interpretation monad
-------------------------------------------------------------------------------}

-- | Interpretation monad
newtype IntT h e m a = IntT {
    unIntT :: StateT (IntCtxt h) (TranslateT (Either IntException e) m) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader TransCtxt
           , MonadError (Either IntException e)
           )

-- | Evaluate state strictly
instance Monad m => MonadState (IntCtxt h) (IntT h e m) where
 get    = IntT $ get
 put !s = IntT $ put s

{-------------------------------------------------------------------------------
  Translate the Abstract chain definitions to Cardano types
-------------------------------------------------------------------------------}

data Abstract2Cardano

instance Interpretation Abstract2Cardano where
  type IntCtx Abstract2Cardano = IntT
