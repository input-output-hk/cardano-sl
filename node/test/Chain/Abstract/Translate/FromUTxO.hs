{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
-- | Translation of UTxO DSL into an abstract chain.
module Chain.Abstract.Translate.FromUTxO where

import Chain.Abstract
import Chain.Policy
import Control.Lens ((%=), ix)
import Control.Lens.TH (makeLenses)
import Control.Monad.Except
import UTxO.Interpreter (Interpret(..), Interpretation(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Universum
import qualified UTxO.DSL as DSL
import qualified Data.Set as Set

{-------------------------------------------------------------------------------
  Translation context
-------------------------------------------------------------------------------}

data TransCtxt = TransCtxt
  { -- | All actors in the system.
    _tcAddresses :: NonEmpty Addr
  }

makeLenses ''TransCtxt

{-------------------------------------------------------------------------------
  Translation into abstract chain
-------------------------------------------------------------------------------}

newtype TranslateT e m a = TranslateT {
      unTranslateT :: ExceptT e (ReaderT TransCtxt m) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError e
           , MonadIO
           , MonadReader TransCtxt
           )

instance MonadTrans (TranslateT e) where
  lift = TranslateT . lift . lift

type Translate e = TranslateT e Identity

-- | Map errors
mapTranslateErrors :: Functor m
                   => (e -> e') -> TranslateT e m a -> TranslateT e' m a
mapTranslateErrors f (TranslateT ma) = TranslateT $ withExceptT f ma

{-------------------------------------------------------------------------------
  Errors that may occur during interpretation
-------------------------------------------------------------------------------}

-- | Interpretation error
data IntException =
    -- | A UTxO transcation has an empty input list.
    IntEmptyInputs
    -- | A UTxO transaction has an empty output list.
  | IntEmptyOutputs
  | IntUnknownHash      Text
  | IntIndexOutOfRange  Text Word32 -- ^ During input resolution (hash and index)
  deriving (Show)

instance Exception IntException

{-------------------------------------------------------------------------------
  Interpretation context
-------------------------------------------------------------------------------}

-- | Checkpoint (we create one for each block we translate)
data IntCheckpoint h = IntCheckpoint {
      -- | Slot number of this checkpoint
      icSlotId        :: !SlotId

      -- | Hash of the current block
    , icBlockHash     :: !(h (Block h Addr))

      -- | Running stakes
    , icStakes        :: !(StakeDistribution Addr)

      -- | Delegation graph. This is instantiated to the identify function.
    , icDlg           :: Addr -> Addr
    }

-- | Interpretation context
data IntCtxt h = IntCtxt {
      -- | Transaction map
      _icTx      :: !(Map (h (DSL.Transaction h Addr)) (Transaction h Addr))

      -- | Checkpoints
    , _icCheckpoints :: !(NonEmpty (IntCheckpoint h))

      -- | Block modifiers
    , _icBlockMods :: ![BlockModifier (IntT h BlockModifierException Identity) h Addr]
    }


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
           , MonadError (Either IntException e)
           )

-- | Evaluate state strictly
instance Monad m => MonadState (IntCtxt h) (IntT h e m) where
  get    = IntT $ get
  put !s = IntT $ put s

-- | Convenience function to lift actions in the 'Translate' monad
liftTranslateInt :: Monad m
                 => TranslateT IntException m a
                 -> IntT h e m a
liftTranslateInt ta =  IntT $ lift $ mapTranslateErrors Left ta

makeLenses ''IntCtxt

{-------------------------------------------------------------------------------
  Dealing with the transactions
-------------------------------------------------------------------------------}

-- | Add transaction into the context
putTx :: forall h e m. (DSL.Hash h Addr, Monad m)
      => Transaction h Addr
      -> IntT h e m ()
putTx t = icTx %= Map.insert (hash t) t

getTx :: (DSL.Hash h Addr, Monad m)
      => h (DSL.Transaction h Addr)
      -> IntT h e m (Transaction h Addr)
getTx h = do
    tx <- use icTx
    case Map.lookup h tx of
      Nothing -> throwError $ Left $ IntUnknownHash (pretty h)
      Just m  -> return m

-- | Lookup a transaction by hash
findHash' :: (DSL.Hash h Addr, Monad m)
          => h (DSL.Transaction h Addr)
          -> IntT h e m (Transaction h Addr)
findHash' = getTx

-- | Resolve an input
inpSpentOutput' :: (DSL.Hash h Addr, Monad m)
                => DSL.Input h Addr
                -> IntT h e m (Output h Addr)
inpSpentOutput' (DSL.Input h idx) =  do
    tx <- findHash' h
    case toList (trOuts tx) ^? ix (fromIntegral idx) of
      Nothing  -> throwError $ Left $ IntIndexOutOfRange (pretty h) idx
      Just out -> return out

{-------------------------------------------------------------------------------
  Interpreter proper
-------------------------------------------------------------------------------}

data DSL2Abstract

instance Interpretation DSL2Abstract where
  type IntCtx DSL2Abstract = IntT

instance Interpret DSL2Abstract h (DSL.Output h Addr) where
  type Interpreted DSL2Abstract (DSL.Output h Addr) = Output h Addr

  int :: Monad m
      => DSL.Output h Addr
      -> IntT h e m (Output h Addr)
  int out = do
    -- Compute the stake repartition function. At present, this is fixed to
    -- assign all stake to the bootstrap stakeholders.
    return $ Output
      { outAddr = DSL.outAddr out
      , outVal = DSL.outVal out
      , outRepartition = Repartition $ Map.empty
      }

-- | Transactions are interpreted through attaching a list of witnesses.
--
--   We also keep a record of all transactions in the interpretation context.
instance DSL.Hash h Addr => Interpret DSL2Abstract h (DSL.Transaction h Addr) where
  type Interpreted DSL2Abstract (DSL.Transaction h Addr) = Transaction h Addr

  int :: forall m e. Monad m
      => DSL.Transaction h Addr
      -> IntT h e m (Transaction h Addr)
  int tr = do
      allAddrs <- liftTranslateInt $ asks _tcAddresses
      outs <- mapM (int @DSL2Abstract) =<< (nonEmptyEx IntEmptyOutputs $ DSL.trOuts tr)
      ins <- nonEmptyEx IntEmptyInputs $ Set.toList $ DSL.trIns tr
      let absTr = Transaction
            { trFresh = DSL.trFresh tr
            , trIns = ins
            , trOuts = outs
            , trFee = DSL.trFee tr
            , trHash = DSL.trHash tr
            , trExtra = DSL.trExtra tr
            , trWitness = allAddrs
            }
      putTx absTr
      return absTr
    where
      nonEmptyEx :: IntException -> [a] -> IntT h e m (NonEmpty a)
      nonEmptyEx ex [] = throwError $ Left $ ex
      nonEmptyEx _ (x:xs) = return $ x :| xs

instance DSL.Hash h Addr => Interpret DSL2Abstract h (DSL.Block h Addr) where
  type Interpreted DSL2Abstract (DSL.Block h Addr) = Block h Addr

  int :: Monad m
      => DSL.Block h Addr
      -> IntT h e m (Block h Addr)
  int block = do
    allAddrs <- liftTranslateInt $ asks _tcAddresses
    c :| _  <- use icCheckpoints
    trs <- mapM  (int @DSL2Abstract) block
    return $ Block
      { blockPred = icBlockHash c
      , blockSlot = icSlotId c
      , blockIssuer = head allAddrs
      , blockTransactions = trs
      , blockDlg = []
      }

instance DSL.Hash h Addr => Interpret DSL2Abstract h (DSL.Chain h Addr) where
  type Interpreted DSL2Abstract (DSL.Chain h Addr) = Chain h Addr

  int :: Monad m
      => DSL.Chain h Addr
      -> IntT h e m (Chain h Addr)
  int = mapM (int @DSL2Abstract)

{-------------------------------------------------------------------------------
  Running the interpreter
-------------------------------------------------------------------------------}

-- | Translate from a UTxO chain into the abstract one.
translate
  :: (DSL.Hash h Addr, Monad m)
     -- | Set of all actors in the system. All transactions will be considered
     -- as existing between these.
  => NonEmpty Addr
     -- | UTxO DSL chain
  -> DSL.Chain h Addr
     -- | Block modifiers. These can be used to tune the chain generation to
     -- give different validating and non-validating chains.
  -> [BlockModifier (IntT h IntException m) h Addr]
  -> m (Either IntException (Chain h Addr))
translate = undefined
