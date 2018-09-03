{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | General purpose utility functions
module Cardano.Wallet.Kernel.Util (
    -- * Lists
    at
  , neHead
  , shuffle
  , shuffleNE
    -- * Maps and sets
  , disjoint
  , withoutKeys
  , restrictKeys
  , markMissingMapEntries
    -- * Dealing with OldestFirst/NewestFirst
  , liftOldestFirstF
  , liftNewestFirstF
  , liftOldestFirst
  , liftNewestFirst
    -- * Probabilities
  , Probability
  , toss
    -- * MonadState utilities
  , modifyAndGetOld
  , modifyAndGetNew
    -- * ExceptT utilities
  , exceptT
    -- * Spaceleak free version of WriterT
  , Collect(..)
  , traverseCollect
    -- * Dealing with Void
  , mustBeRight
    -- * Compression
  , MapDiff(..)
  , deltaMap
  , stepMap
  ) where

import           Universum

import           Control.Monad.Except (MonadError (..))
import           Crypto.Number.Generate (generateBetween)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Merge.Strict as Map.Merge
import qualified Data.Map.Strict as Map
import qualified Data.SafeCopy as SC
import qualified Data.Set as Set
import qualified Data.Vector as V
import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MV
import           Pos.Core.Chrono
import qualified Test.QuickCheck as QC

{-------------------------------------------------------------------------------
  Lists
-------------------------------------------------------------------------------}

-- | Safe version of (!!)
at :: [a] -> Int -> Maybe a
at []     _ = Nothing
at (x:_)  0 = Just x
at (_:xs) i = at xs (i - 1)

neHead :: Lens' (NonEmpty a) a
neHead f (x :| xs) = (:| xs) <$> f x

shuffle :: [a] -> IO [a]
shuffle = modifyInPlace $ \v -> do
    let (lo, hi) = (0, MV.length v - 1)
    forM_ [lo .. hi] $ \i -> do
      j <- fromInteger <$> generateBetween (fromIntegral lo) (fromIntegral hi)
      swapElems v i j
  where
    swapElems :: IOVector a -> Int -> Int -> IO ()
    swapElems v i j = do
        x <- MV.read v i
        y <- MV.read v j
        MV.write v i y
        MV.write v j x

shuffleNE :: NonEmpty a -> IO (NonEmpty a)
shuffleNE = fmap NE.fromList . shuffle . NE.toList

modifyInPlace :: forall a. (IOVector a -> IO ()) -> [a] -> IO [a]
modifyInPlace f xs = do
    v' <- V.thaw $ V.fromList xs
    f v'
    V.toList <$> V.freeze v'

{-------------------------------------------------------------------------------
  Maps and sets
-------------------------------------------------------------------------------}

-- | Check that two sets are disjoint
--
-- This is available out of the box from containters >= 0.5.11
disjoint :: Ord a => Set a -> Set a -> Bool
disjoint a b = Set.null (a `Set.intersection` b)

withoutKeys :: Ord k => Map k a -> Set k -> Map k a
m `withoutKeys` s = m `Map.difference` Map.fromSet (const ()) s

restrictKeys :: Ord k => Map k a -> Set k -> Map k a
m `restrictKeys` s = m `Map.intersection` Map.fromSet (const ()) s

-- | @markMissingMapEntries mustExist@ adds a 'Nothing' value for each key
-- in @mustExist@ that doesn't exist in the input map.
markMissingMapEntries :: Ord k => Map k b -> Map k a -> Map k (Maybe a)
markMissingMapEntries =
    Map.Merge.merge
      (Map.Merge.mapMaybeMissing     $ \_k _b   -> Just $ Nothing)
      (Map.Merge.mapMaybeMissing     $ \_k    a -> Just $ Just a)
      (Map.Merge.zipWithMaybeMatched $ \_k _b a -> Just $ Just a)

{-------------------------------------------------------------------------------
  Dealing with OldestFirst/NewestFirst
-------------------------------------------------------------------------------}

liftOldestFirstF :: Functor m
                 => (f a -> m (f a))
                 -> OldestFirst f a -> m (OldestFirst f a)
liftOldestFirstF f = fmap OldestFirst . f . getOldestFirst

liftNewestFirstF :: Functor m
                 => (f a -> m (f a))
                 -> NewestFirst f a -> m (NewestFirst f a)
liftNewestFirstF f = fmap NewestFirst . f . getNewestFirst

liftOldestFirst :: (f a -> f a) -> OldestFirst f a -> OldestFirst f a
liftOldestFirst f = runIdentity . liftOldestFirstF (Identity . f)

liftNewestFirst :: (f a -> f a) -> NewestFirst f a -> NewestFirst f a
liftNewestFirst f = runIdentity . liftNewestFirstF (Identity . f)

{-------------------------------------------------------------------------------
  Probabilities
-------------------------------------------------------------------------------}

-- | Probability (value between 0 and 1)
type Probability = Double

-- | Weighted coin toss
--
-- @toss p@ throws a p-weighted coin and returns whether it came up heads.
-- @toss 0@ will always return @False@, @toss 1@ will always return @True@.
toss :: Probability -> QC.Gen Bool
toss 0 = return False
toss 1 = return True
toss p = (< p) <$> QC.choose (0, 1)

{-------------------------------------------------------------------------------
  MonadState util
-------------------------------------------------------------------------------}

-- | Modify the state and return the new state
modifyAndGetNew :: MonadState s m => (s -> s) -> m s
modifyAndGetNew f = state $ \old -> let new = f old in (new, new)

-- | Modify the state and return the old state
modifyAndGetOld :: MonadState s m => (s -> s) -> m s
modifyAndGetOld f = state $ \old -> let new = f old in (old, new)

{-------------------------------------------------------------------------------
  ExceptT utilities
-------------------------------------------------------------------------------}

exceptT :: Monad m => Either e a -> ExceptT e m a
exceptT (Left  e) = throwError e
exceptT (Right a) = return a

{-------------------------------------------------------------------------------
  Spaceleak free version of WriterT
-------------------------------------------------------------------------------}

-- | Applicative-only, strict, spaceleak-free version of 'WriterT'
newtype Collect w f a = Collect { runCollect :: f (a, w) }

deriving instance Show (f (a, w)) => Show (Collect w f a)

instance Functor f => Functor (Collect w f) where
  fmap f (Collect bcs) = Collect (fmap (first f) bcs)

instance (Applicative f, Monoid w) => Applicative (Collect w f) where
  pure x = Collect (pure (x, mempty))
  Collect fcs <*> Collect bcs = Collect (aux <$> fcs <*> bcs)
    where
      -- We force the evaluation of both logs, and tie the evaluation of their
      -- concatenation of the pair also, just to be sure to be sure
      aux :: (a -> b, w) -> (a, w) -> (b, w)
      aux (f, !w) (a, !w') = let !w'' = mappend w w' in (f a, w'')

-- | Walk over a traversable data structure, collecting additional results
traverseCollect :: forall t f a b c. (Traversable t, Applicative f)
                => (a -> f (b, c)) -> t a -> f (t b, [c])
traverseCollect f = runCollect . traverse f'
  where
    f' :: a -> Collect [c] f b
    f' = Collect . fmap (second (:[])) . f

{-------------------------------------------------------------------------------
  Dealing with Void
-------------------------------------------------------------------------------}

mustBeRight :: Either Void b -> b
mustBeRight (Left  a) = absurd a
mustBeRight (Right b) = b

{-------------------------------------------------------------------------------
  Compression
-------------------------------------------------------------------------------}


-- As a diff of two Maps we use the Map of new values (changed or completely new)
-- plus a Set of deleted values.
data MapDiff k v = MapDiff {
      mapDiffAdded   :: Map.Map k v
    , setDiffDeleted :: Set.Set k
  }

-- property: keys of the return set cannot be keys of the returned Map.
deltaMap :: (Eq v, Ord k) => Map k v -> Map k v -> MapDiff k v
deltaMap newMap oldMap =
  let f newEntry oldEntry = if newEntry == oldEntry then Nothing else Just newEntry
      newEntries = Map.differenceWith f newMap oldMap -- this includes pairs that changed values.
      deletedKeys = Map.keysSet $ Map.difference oldMap newMap
  in MapDiff newEntries deletedKeys

-- newEntries should have no keys in common with deletedKeys.
stepMap :: Ord k => Map k v -> MapDiff k v -> Map k v
stepMap oldMap (MapDiff newEntries deletedKeys) =
  Map.union newEntries lighterMap -- for common keys, union prefers the newPairs values.
    where lighterMap = Map.withoutKeys oldMap deletedKeys

instance (SC.SafeCopy k, SC.SafeCopy v, Ord k) => SC.SafeCopy (MapDiff k v) where
  getCopy = SC.contain $ do
    (m, s) <- SC.safeGet
    pure $ MapDiff m s
  putCopy (MapDiff m s)  = SC.contain $ do
    SC.safePut (m, s)
