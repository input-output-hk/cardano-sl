{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- | Translation of UTxO DSL into an abstract chain.
module Chain.Abstract.Translate.FromUTxO where

import           Chain.Abstract
import           Chain.Policy
import           Control.Lens (ix, (%=), (<%=))
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Except
import           Data.List.NonEmpty ((<|))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Pos.Core.Chrono (OldestFirst (..))
import           Universum
import qualified UTxO.DSL as DSL

import           Chain.Abstract.Repartition (mkRepartitionT)

{-------------------------------------------------------------------------------
  Chain validity
-------------------------------------------------------------------------------}

data ChainValidity
    = ValidChain
    | InvalidChain [(SlotId, PolicyViolation)]
    deriving (Show)

{-------------------------------------------------------------------------------
  Translation context
-------------------------------------------------------------------------------}

-- | Checkpoint (we create one for each block we translate).
--
-- TODO: QUESTION: does it make sense to explain the idea behind the
-- 'IntCheckPoint' data type? My guess is that it serves as a sort of cache for
-- information that is already contained in the blockchain.
data IntCheckpoint = IntCheckpoint {
      -- | Slot number of this checkpoint
      icSlotId    :: !SlotId

      -- | Hash of the current block
    , icBlockHash :: !BlockHash

      -- | Running stakes
    , icStakes    :: !(StakeDistribution Addr)

      -- | Delegation graph. This is instantiated to the identity function.
    , icDlg       :: Addr -> Addr
    }

-- | Translation state
data TransState h = TransState {
      -- | Transaction map
      _tsTx          :: !(Map (h (DSL.Transaction h Addr)) (Transaction h Addr))
      -- | Current slot ID. Note that this may not be the same as the increment
      -- of the previous checkpoint, because it's possible that no block gets
      -- issued in a slot.
    , _tsCurrentSlot :: !SlotId
      -- | Checkpoints
    , _tsCheckpoints :: !(NonEmpty IntCheckpoint)
    }

makeLenses ''TransState

-- | Translation context. This the read-only data available to translation.
data TransCtxt h = TransCtxt
  { -- | All actors in the system.
    _tcAddresses :: NonEmpty Addr
  , _parameters  :: Parameters (TransState h) h Addr
  }

makeLenses ''TransCtxt

{-------------------------------------------------------------------------------
  Errors that may occur during interpretation
-------------------------------------------------------------------------------}

-- | Interpretation error
data IntException =
    -- | A UTxO transaction has an empty input list.
    IntEmptyInputs
    -- | A UTxO transaction has an empty output list.
  | IntEmptyOutputs
  | IntUnknownHash      Text
  | IntIndexOutOfRange  Text Word32 -- ^ During input resolution (hash and index)
    -- | A DSL chain has an empty set of addresses.
  | IntEmptyAddresses
    -- | Some error with the interpretation logic
    -- TODO: if this makes sense we'd want to add more information.
  | IntLogicError
  deriving (Show, Eq)

instance Exception IntException

{-------------------------------------------------------------------------------
  Translation into abstract chain
-------------------------------------------------------------------------------}

newtype TranslateT h e m a = TranslateT {
      unTranslateT :: StateT (TransState h) (ReaderT (TransCtxt h) (ExceptT e m)) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError e
           , MonadIO
           , MonadReader (TransCtxt h)
           , MonadState (TransState h)
           )

instance MonadTrans (TranslateT h e) where
    lift = TranslateT . lift . lift . lift
-- | Run a translation given a context and initial state.
runTranslateT :: TransCtxt h
              -> TransState h
              -> TranslateT h e m a
              -> ExceptT e m (a, TransState h)
runTranslateT ctxt st (TranslateT stAct) = flip runReaderT ctxt $ runStateT stAct st

{-------------------------------------------------------------------------------
  Dealing with the transactions
-------------------------------------------------------------------------------}

-- | Add transaction into the context
putTx :: forall h m. (DSL.Hash h Addr, Monad m)
      => Transaction h Addr
      -> TranslateT h IntException m ()
putTx t = tsTx %= Map.insert (hash t) t

getTx :: (DSL.Hash h Addr, Monad m)
      => h (DSL.Transaction h Addr)
      -> TranslateT h IntException m (Transaction h Addr)
getTx h = do
    tx <- use tsTx
    case Map.lookup h tx of
      Nothing -> throwError $ IntUnknownHash (pretty h)
      Just m  -> return m

-- | Lookup a transaction by hash
findHash' :: (DSL.Hash h Addr, Monad m)
          => h (DSL.Transaction h Addr)
          -> TranslateT h IntException m (Transaction h Addr)
findHash' = getTx

-- | Resolve an input
inpSpentOutput' :: (DSL.Hash h Addr, Monad m)
                => DSL.Input h Addr
                -> TranslateT h IntException m (Output h Addr)
inpSpentOutput' (DSL.Input h idx) =  do
    tx <- findHash' h
    case toList (trOuts tx) ^? ix (fromIntegral idx) of
      Nothing  -> throwError $ IntIndexOutOfRange (pretty h) idx
      Just out -> return out

{-------------------------------------------------------------------------------
  Running the interpreter
-------------------------------------------------------------------------------}

-- | Translate from a UTxO chain into the abstract one.
translate
  :: forall h m. (DSL.Hash h Addr, Monad m)
     -- | Set of all actors in the system. All transactions will be considered
     -- as existing between these.
  => NonEmpty Addr
     -- | UTxO DSL chain
  -> DSL.Chain h Addr
     -- | Policies. These can be used to tune the chain generation to
     -- give different validating and non-validating chains.
  -> [Policy h (TranslateT h IntException m)]
  -> Parameters (TransState h) h Addr
  -> m (Either IntException (Chain h Addr, ChainValidity))
translate addrs chain policies params = runExceptT . fmap fst $ runTranslateT initCtx initState go
  where
    initCtx = TransCtxt
        { _tcAddresses = addrs
        , _parameters = params
        }
    initState = TransState
        { _tsTx = Map.empty
        , _tsCheckpoints = initCheckpoint :| []
        , _tsCurrentSlot = SlotId 0
        }
    initCheckpoint = IntCheckpoint
        { icSlotId = SlotId 0
        , icBlockHash = genesisBlockHash
        , icStakes = StakeDistribution $ Map.fromList (toList addrs `zip` (repeat 1))
        , icDlg = id
        }
    go :: TranslateT h IntException m (Chain h Addr, ChainValidity)
    go = do
        chainWithViolations <- mapM intBlock chain
        return $ (fst <$> chainWithViolations, checkValid chainWithViolations)
      where
        checkValid (OldestFirst blks) = let
            tagLoc (block, pvs) = fmap (blockSlot block, ) pvs
          in case tagLoc =<< blks of
            [] -> ValidChain
            xs -> InvalidChain xs

    intOutput
      :: (Ord a, MonadError IntException n)
      => DSL.Output h a -> n (Output h1 a)
    intOutput out =  do
        r <- mkRepartitionT (const IntLogicError) [(DSL.outAddr out, DSL.outVal out)]
        -- Compute the stake repartition function. At present, this is fixed to
        -- assign all stake to the bootstrap stakeholders.
        return $ Output
          { outAddr = DSL.outAddr out
          , outVal = DSL.outVal out
          , outRepartition = r
          }

    intTransaction :: DSL.Transaction h Addr -> TranslateT h IntException m (Transaction h Addr)
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

    intBlock :: DSL.Block h Addr -> TranslateT h IntException m (Block h Addr, [PolicyViolation])
    intBlock block = do
      allAddrs <- asks _tcAddresses
      curSlot <- tsCurrentSlot <%= nextSlot
      c :| _  <- use tsCheckpoints
      trs <- mapM intTransaction block
      -- TODO Create checkpoint!
      r@(bl, _) <- applyBlockMod (mconcat $ polGenerator <$> policies) $ Block
        { blockPred = icBlockHash c
        , blockSlot = curSlot
        , blockIssuer = head allAddrs
        , blockTransactions = trs
        , blockDlg = []
        }
      createCheckpoint bl
      return r
    nonEmptyEx :: IntException -> [a] -> TranslateT h IntException m (NonEmpty a)
    nonEmptyEx ex []    = throwError $ ex
    nonEmptyEx _ (x:xs) = return $ x :| xs
    applyBlockMod (BlockModifier gen) block = gen block
    createCheckpoint :: Block h Addr -> TranslateT h IntException m ()
    createCheckpoint block = modify $ over tsCheckpoints (ic <|)
        where
            ic = IntCheckpoint
                { icSlotId = blockSlot block
                , icBlockHash = blockHash block
                , icStakes = initialStakeDistribution params
                , icDlg = id
                }
