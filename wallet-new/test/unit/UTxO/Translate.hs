{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UTxO.Translate (
    -- * Monadic context for the translation from the DSL to Cardano
    TranslateT
  , Translate
  , runTranslateT
  , runTranslateTNoErrors
  , runTranslate
  , runTranslateNoErrors
  , withConfig
  , mapTranslateErrors
  , catchTranslateErrors
  , catchSomeTranslateErrors
    -- * Convenience wrappers
  , translateFirstSlot
  , translateNextSlot
  , translateGenesisHeader
    -- * Interface to the verifier
  , verify
  , verifyBlocksPrefix
    -- * Convenience re-exports
  , MonadError(..)
  , MonadGState(..)
  ) where

import           Control.Exception (throw)
import           Control.Monad.Except
import           Data.Constraint (Dict (..))
import           Universum

import           Pos.Chain.Block
import           Pos.Chain.Txp
import           Pos.Chain.Update
import           Pos.Core
import           Pos.Core.Chrono
import           Pos.DB.Class (MonadGState (..))

import           Util.Validated
import           UTxO.Context
import           UTxO.Verify (Verify)
import qualified UTxO.Verify as Verify

import           Test.Pos.Core.Dummy (dummyBlockVersionData, dummyEpochSlots)

{-------------------------------------------------------------------------------
  Testing infrastructure from cardano-sl-core

  The genesis block comes from defaultTestConf, which in turn uses
  configuration.yaml. It is specified by a 'GenesisSpec'.
-------------------------------------------------------------------------------}

import           Test.Pos.Configuration (withDefConfiguration,
                     withDefUpdateConfiguration)

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
    , teUpdate  :: Dict HasUpdateConfiguration
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

-- | Right now this always returns the genesis policy
instance Monad m => MonadGState (TranslateT e m) where
  gsAdoptedBVData = pure dummyBlockVersionData

-- | Run translation
--
-- NOTE: This uses the default test configuration, and throws any errors as
-- pure exceptions.
runTranslateT :: Monad m => Exception e => TranslateT e m a -> m a
runTranslateT (TranslateT ta) =
    withDefConfiguration $ \coreConfig ->
    withDefUpdateConfiguration $
      let env :: TranslateEnv
          env = TranslateEnv {
                    teContext = initContext (initCardanoContext coreConfig)
                  , teUpdate  = Dict
                  }
      in do ma <- runReaderT (runExceptT ta) env
            case ma of
              Left  e -> throw  e
              Right a -> return a

-- | Specialised form of 'runTranslateT' when there can be no errors
runTranslateTNoErrors :: Monad m => TranslateT Void m a -> m a
runTranslateTNoErrors = runTranslateT

-- | Specialization of 'runTranslateT'
runTranslate :: Exception e => Translate e a -> a
runTranslate = runIdentity . runTranslateT

-- | Specialised form of 'runTranslate' when there can be no errors
runTranslateNoErrors :: Translate Void a -> a
runTranslateNoErrors = runTranslate

-- | Lift functions that want the configuration as type class constraints
withConfig :: Monad m
           => (HasUpdateConfiguration => TranslateT e m a)
           -> TranslateT e m a
withConfig f = do
    Dict <- TranslateT $ asks teUpdate
    f

-- | Map errors
mapTranslateErrors :: Functor m
                   => (e -> e') -> TranslateT e m a -> TranslateT e' m a
mapTranslateErrors f (TranslateT ma) = TranslateT $ withExceptT f ma

-- | Catch and return errors
catchTranslateErrors :: Functor m
                     => TranslateT e m a -> TranslateT e' m (Either e a)
catchTranslateErrors (TranslateT (ExceptT (ReaderT ma))) =
    TranslateT $ ExceptT $ ReaderT $ \env -> fmap Right (ma env)

catchSomeTranslateErrors :: Monad m
                         => TranslateT (Either e e') m a
                         -> TranslateT e m (Either e' a)
catchSomeTranslateErrors act = do
    ma <- catchTranslateErrors act
    case ma of
      Left (Left e)   -> throwError e
      Left (Right e') -> return $ Left e'
      Right a         -> return $ Right a

{-------------------------------------------------------------------------------
  Convenience wrappers
-------------------------------------------------------------------------------}

-- | Slot ID of the first block
translateFirstSlot :: SlotId
translateFirstSlot = SlotId 0 localSlotIndexMinBound

-- | Increment slot ID
--
-- TODO: Surely a function like this must already exist somewhere?
translateNextSlot :: Monad m => SlotId -> TranslateT e m SlotId
translateNextSlot (SlotId epoch lsi) = withConfig $
    return $ case addLocalSlotIndex dummyEpochSlots 1 lsi of
               Just lsi' -> SlotId epoch       lsi'
               Nothing   -> SlotId (epoch + 1) localSlotIndexMinBound

-- | Genesis block header
translateGenesisHeader :: Monad m => TranslateT e m GenesisBlockHeader
translateGenesisHeader = view gbHeader <$> asks (ccBlock0 . tcCardano)

{-------------------------------------------------------------------------------
  Interface to the verifier
-------------------------------------------------------------------------------}

-- | Run the verifier
verify :: Monad m
       => Verify e a
       -> TranslateT e' m (Validated e (a, Utxo))
verify ma = withConfig $ do
    utxo <- asks (ccUtxo . tcCardano)
    return $ validatedFromEither (Verify.verify utxo ma)

-- | Wrapper around 'UTxO.Verify.verifyBlocksPrefix'
--
-- NOTE: This assumes right now that we verify starting from the genesis block.
-- If that assumption is not valid, we need to pass in the slot leaders here.
-- Probably easier is to always start from an epoch boundary block; in such a
-- case in principle we don't need to pass in the set of leaders all, since the
-- the core of the block verification code ('verifyBlocks' from module
-- "Pos.Chain.Block") will then take the set of leaders from the
-- genesis/epoch boundary block itself. In practice, however, the passed in set
-- of leaders is verified 'slogVerifyBlocks', so we'd have to modify the
-- verification code a bit.

--   It is not required that all blocks are from the same epoch. This will
--   split the chain into epochs and validate each epoch individually
verifyBlocksPrefix
  :: forall e' m.  Monad m
  => OldestFirst NE Block
  -> TranslateT e' m (Validated VerifyBlocksException (OldestFirst NE Undo, Utxo))
verifyBlocksPrefix blocks =
    case splitEpochs blocks of
      ESREmptyEpoch _          ->
        validatedFromExceptT . throwError $ VerifyBlocksError "Whoa! Empty epoch!"
      ESRStartsOnBoundary _    ->
        validatedFromExceptT . throwError $ VerifyBlocksError "No genesis epoch!"
      ESRValid genEpoch (OldestFirst succEpochs) -> do
        CardanoContext{..} <- asks tcCardano
        verify $ validateGenEpoch ccHash0 ccInitLeaders genEpoch >>= \genUndos -> do
          epochUndos <- sequence $ validateSuccEpoch <$> succEpochs
          return $ foldl' (\a b -> a <> b) genUndos epochUndos

  where
    validateGenEpoch :: HeaderHash
                     -> SlotLeaders
                     -> OldestFirst NE MainBlock
                     -> Verify VerifyBlocksException (OldestFirst NE Undo)
    validateGenEpoch ccHash0 ccInitLeaders geb = do
      Verify.verifyBlocksPrefix
        ccHash0
        Nothing
        ccInitLeaders
        (OldestFirst [])
        (Right <$> geb ::  OldestFirst NE Block)
    validateSuccEpoch :: EpochBlocks NE
                      -> Verify VerifyBlocksException (OldestFirst NE Undo)
    validateSuccEpoch (SuccEpochBlocks ebb emb) = do
      Verify.verifyBlocksPrefix
        (ebb ^. headerHashG)
        Nothing
        (ebb ^. gbBody . gbLeaders)
        (OldestFirst []) -- ^ TODO pass these?
        (Right <$> emb)

-- | Blocks inside an epoch
data EpochBlocks a = SuccEpochBlocks !GenesisBlock !(OldestFirst a MainBlock)

-- | Try to convert the epoch into a non-empty epoch
neEpoch :: EpochBlocks [] -> Maybe (EpochBlocks NE)
neEpoch (SuccEpochBlocks ebb (OldestFirst mbs)) = case nonEmpty mbs of
  Nothing   -> Nothing
  Just mbs' -> Just $ SuccEpochBlocks ebb (OldestFirst mbs')

-- | Epoch splitting result. This validates that the chain follow the pattern
--   `m(m*)(b(m+))*`, where `m` denotes a main block, and `b` a boundary block.
--
--   Either we have a valid splitting of the chain into epochs, or at some point
--   we have an empty epoch, in which case we return the successive boundary
--   blocks, or we have that the chain starts on a boundary block.
--
--   This does not catch the case where blocks are in the wrong epoch, which
--   will be detected by slot leaders being incorrect.
data EpochSplitResult
  = ESRValid !(OldestFirst NE MainBlock) !(OldestFirst [] (EpochBlocks NE))
  | ESREmptyEpoch !GenesisBlock
  | ESRStartsOnBoundary !GenesisBlock

-- | Split a non-empty set of blocks into the genesis epoch (which does not start with
--   an epoch boundary block) and a set of successive epochs, starting with an EBB.
splitEpochs :: OldestFirst NE Block
            -> EpochSplitResult
splitEpochs blocks = case spanEpoch blocks of
  (Left (SuccEpochBlocks ebb _), _) -> ESRStartsOnBoundary ebb
  (Right genEpoch, OldestFirst rem') -> go [] (OldestFirst <$> nonEmpty rem') where
    go !acc Nothing = ESRValid genEpoch (OldestFirst $ reverse acc)
    go !acc (Just neRem) = case spanEpoch neRem of
      (Right _, _) -> error "Impossible!"
      (Left ebs@(SuccEpochBlocks ebb _), OldestFirst newRem) -> case neEpoch ebs of
        Nothing   -> if null newRem
                     then ESRValid genEpoch (OldestFirst $ reverse acc)
                     else ESREmptyEpoch ebb
        Just ebs' -> go (ebs' :  acc) (OldestFirst <$> nonEmpty newRem)

-- | Span the epoch until the next epoch boundary block.
--
--   - If the list of blocks starts with an EBB, this will return 'Left
--     EpochBlocks' containing the EBB and the successive blocks.
--   - If the list of blocks does not start with an EBB, this will return
--     `Right (OldestFirst NE Block)` containg all blocks before the next
--     EBB.
--
--   In either case, it will also return any remainng blocks, which will either be
--   empty or start with an EBB.
spanEpoch :: OldestFirst NE Block
          -> (Either (EpochBlocks []) (OldestFirst NE MainBlock), OldestFirst [] Block)
spanEpoch (OldestFirst (x:|xs)) = case x of
  Left ebb -> over _1 (Left . SuccEpochBlocks ebb . OldestFirst)
              . over _2 OldestFirst
              $ spanMaybe rightToMaybe xs
  -- Take until we find an EBB
  Right mb -> over _1 (Right . OldestFirst . (mb :|))
       . over _2 OldestFirst
       $ spanMaybe rightToMaybe xs

-- | Returns the maximal prefix of the input list mapped to `Just`, along with
-- the remainder.
spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe = go [] where
  go !acc _ [] = (reverse acc, [])
  go !acc f (x:xs) = case f x of
    Just x' -> go (x':acc) f xs
    Nothing -> (reverse acc, x:xs)
