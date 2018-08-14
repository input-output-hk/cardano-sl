{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
-- | Translation of UTxO DSL into an abstract chain.
module Chain.Abstract.Translate.FromUTxO where

import Chain.Abstract
import Chain.Policy
import Control.Lens ((%=), ix)
import Control.Lens.TH (makeLenses)
import Control.Monad.Except
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Universum
import qualified UTxO.DSL as DSL
import qualified Data.Set as Set

{-------------------------------------------------------------------------------
  Translation context
-------------------------------------------------------------------------------}

-- | Translation context. This the read-only data available to translation.
data TransCtxt = TransCtxt
  { -- | All actors in the system.
    _tcAddresses :: NonEmpty Addr
  }

makeLenses ''TransCtxt

-- | Checkpoint (we create one for each block we translate)
data IntCheckpoint = IntCheckpoint {
      -- | Slot number of this checkpoint
      icSlotId        :: !SlotId

      -- | Hash of the current block
    , icBlockHash     :: !BlockHash

      -- | Running stakes
    , icStakes        :: !(StakeDistribution Addr)

      -- | Delegation graph. This is instantiated to the identify function.
    , icDlg           :: Addr -> Addr
    }

-- | Translation state
data TransState h = TransState {
      -- | Transaction map
      _tsTx      :: !(Map (h (DSL.Transaction h Addr)) (Transaction h Addr))

      -- | Checkpoints
    , _tsCheckpoints :: !(NonEmpty IntCheckpoint)
    }

makeLenses ''TransState

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
  Translation into abstract chain
-------------------------------------------------------------------------------}

newtype TranslateT h e m a = TranslateT {
      unTranslateT :: StateT (TransState h) (ReaderT TransCtxt (ExceptT (Either IntException e) m)) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError (Either IntException e)
           , MonadIO
           , MonadReader TransCtxt
           , MonadState (TransState h)
           )

-- | Run a translation given a context and initial state.
runTranslateT :: TransCtxt
              -> TransState h
              -> TranslateT h e m a
              -> ExceptT (Either IntException e) m (a, TransState h)
runTranslateT ctxt st (TranslateT stAct) = flip runReaderT ctxt $ runStateT stAct st

{-------------------------------------------------------------------------------
  Dealing with the transactions
-------------------------------------------------------------------------------}

-- | Add transaction into the context
putTx :: forall h e m. (DSL.Hash h Addr, Monad m)
      => Transaction h Addr
      -> TranslateT h e m ()
putTx t = tsTx %= Map.insert (hash t) t

getTx :: (DSL.Hash h Addr, Monad m)
      => h (DSL.Transaction h Addr)
      -> TranslateT h e m (Transaction h Addr)
getTx h = do
    tx <- use tsTx
    case Map.lookup h tx of
      Nothing -> throwError $ Left $ IntUnknownHash (pretty h)
      Just m  -> return m

-- | Lookup a transaction by hash
findHash' :: (DSL.Hash h Addr, Monad m)
          => h (DSL.Transaction h Addr)
          -> TranslateT h e m (Transaction h Addr)
findHash' = getTx

-- | Resolve an input
inpSpentOutput' :: (DSL.Hash h Addr, Monad m)
                => DSL.Input h Addr
                -> TranslateT h e m (Output h Addr)
inpSpentOutput' (DSL.Input h idx) =  do
    tx <- findHash' h
    case toList (trOuts tx) ^? ix (fromIntegral idx) of
      Nothing  -> throwError $ Left $ IntIndexOutOfRange (pretty h) idx
      Just out -> return out

{-------------------------------------------------------------------------------
  Running the interpreter
-------------------------------------------------------------------------------}

-- | Translate from a UTxO chain into the abstract one.
translate
  :: forall h m e. (DSL.Hash h Addr, Monad m)
     -- | Set of all actors in the system. All transactions will be considered
     -- as existing between these.
  => NonEmpty Addr
     -- | UTxO DSL chain
  -> DSL.Chain h Addr
     -- | Block modifiers. These can be used to tune the chain generation to
     -- give different validating and non-validating chains.
  -> [BlockModifier (TranslateT h IntException m) h Addr]
  -> m (Either (Either IntException e) (Chain h Addr))
translate addrs chain blockMods = runExceptT . fmap fst $ runTranslateT initCtx initState go
  where
    initCtx = TransCtxt { _tcAddresses = addrs }
    initState = TransState
        { _tsTx = Map.empty
        , _tsCheckpoints = initCheckpoint :| []
        }
    initCheckpoint = IntCheckpoint
        { icSlotId = SlotId 0
        , icBlockHash = BlockHash 0
        , icStakes = StakeDistribution $ Map.fromList (toList addrs `zip` (repeat 1))
        , icDlg = id
        }
    go = mapM intBlock chain
    intOutput out =  do
        -- Compute the stake repartition function. At present, this is fixed to
        -- assign all stake to the bootstrap stakeholders.
        return $ Output
          { outAddr = DSL.outAddr out
          , outVal = DSL.outVal out
          , outRepartition = Repartition $ Map.empty
          }
    intTransaction tr = do
      allAddrs <- asks _tcAddresses
      outs <- mapM intOutput =<< (nonEmptyEx IntEmptyOutputs $ DSL.trOuts tr)
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
    intBlock block = do
      allAddrs <- asks _tcAddresses
      c :| _  <- use tsCheckpoints
      trs <- mapM intTransaction block
      return $ Block
        { blockPred = icBlockHash c
        , blockSlot = icSlotId c
        , blockIssuer = head allAddrs
        , blockTransactions = trs
        , blockDlg = []
        }
    nonEmptyEx :: IntException -> [a] -> TranslateT h e m (NonEmpty a)
    nonEmptyEx ex [] = throwError $ Left $ ex
    nonEmptyEx _ (x:xs) = return $ x :| xs
