{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Translation of an abstract chain into Cardano.
module Chain.Abstract.Translate.ToCardano (
    IntT
  ) where

import           Chain.Abstract (Output, Transaction(..), hash)
import           Control.Lens ((%=), ix)
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Except
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Pos.Core.Common (Coin(..), mkCoin)
import           Pos.Chain.Txp (TxOutAux, TxId, TxIn(..))
import           Universum
import           UTxO.Context (Addr, AddrInfo(..), TransCtxt(..), resolveAddr)
import           UTxO.Crypto (SomeKeyPair, TxOwnedInput)
import qualified UTxO.DSL as DSL (Hash, Input(..), Transaction, Value, outAddr)
import           UTxO.Interpreter (
                   Interpretation(..)
                 , Interpret(..)
                 )

{-------------------------------------------------------------------------------
  Interpretation context
-------------------------------------------------------------------------------}

-- | Checkpoint (we create one for each block we translate)
data IntCheckpoint

-- | Information we keep about the transactions we encounter
data TxMeta h = TxMeta {
  -- | The DSL transaction itself
  --
  -- We use this for input resolution.
  tmTx   :: !(Transaction h Addr)

  -- | Its Cardano hash
  --
  -- This is intentionally not a strict field because for the bootstrap
  -- transaction there is no corresponding TxId and this will be an error term.
, tmHash :: TxId
}

-- | Interpretation context
data IntCtxt h = IntCtxt {
  -- | Transaction map
  _icTx          :: !(Map (h (DSL.Transaction h Addr)) (TxMeta h))
}

makeLenses ''IntCtxt

{-------------------------------------------------------------------------------
Errors that may occur during interpretation
-------------------------------------------------------------------------------}

-- | Interpretation error
data IntException
  = IntUnknownHash     Text
  | IntIndexOutOfRange Text Word32 -- ^ During input resolution (hash and index)
    deriving Show -- TODO

instance Exception IntException

{-------------------------------------------------------------------------------
  Translation monad

  The translation provides access to the translation context as well as some
  dictionaries so that we can lift Cardano operations to the 'Translate' monad.
  (Eventually we may wish to do this differently.)
-------------------------------------------------------------------------------}

-- | Translation environment
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
  Dealing with the transactions
-------------------------------------------------------------------------------}

-- | Add transaction into the context
putTx :: (DSL.Hash h Addr, Monad m)
      => Transaction h Addr
      -> TxId
      -> IntT h e m ()
putTx t txId = icTx %= Map.insert (hash t) TxMeta {
  tmTx   = t
, tmHash = txId
}

-- | Get transaction from the context
getTx :: (DSL.Hash h Addr, Monad m)
      => h (DSL.Transaction h Addr)
      -> IntT h IntException m (TxMeta h)
getTx h = do
    txMap <- use icTx
    case Map.lookup h txMap of
      Nothing     -> throwError $ Left $ IntUnknownHash (pretty h)
      Just txMeta -> return txMeta

-- | Resolve an input
inpSpentOutput' :: (DSL.Hash h Addr, Monad m)
                => DSL.Input h Addr
                -> IntT h IntException m (Output h Addr)
inpSpentOutput' (DSL.Input h idx) =  do
  txMeta <- getTx h
  case toList (trOuts . tmTx $ txMeta) ^? ix (fromIntegral idx) of
    Nothing  -> throwError $ Left $ IntIndexOutOfRange (pretty h) idx
    Just out -> return out

{-------------------------------------------------------------------------------
  Translate the Abstract chain definitions to Cardano types
-------------------------------------------------------------------------------}

data Abstract2Cardano

instance Interpretation Abstract2Cardano where
  type IntCtx Abstract2Cardano = IntT

{-------------------------------------------------------------------------------
  Instances that read, but not update, the state
-------------------------------------------------------------------------------}

instance Interpret Abstract2Cardano h DSL.Value where
  type Interpreted Abstract2Cardano DSL.Value = Coin

  int :: Monad m => DSL.Value -> IntT h e m Coin
  int = return . mkCoin
