{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module Pos.Util.Util
       (
       -- * Existential types
         Some(..)
       , Some1(..)
       , applySome
       , liftLensSome
       , liftGetterSome

       , maybeThrow
       , eitherToFail
       , getKeys
       , sortWithMDesc
       , leftToPanic

       -- * Lenses
       , _neHead
       , _neTail
       , _neLast

       -- * Ether
       , ether
       , Ether.TaggedTrans

       -- * Lifting monads
       , PowerLift(..)

       -- * Asserts
       , inAssertMode

       -- * Concurrency
       , clearMVar
       , forcePutMVar
       , readMVarConditional
       , readUntilEqualMVar
       , readTVarConditional
       , readUntilEqualTVar
       , withReadLifted
       , withWriteLifted

       -- * Instances
       -- ** Lift Byte
       -- ** FromJSON Byte
       -- ** ToJSON Byte
       -- ** MonadFail (Either s), assuming IsString s
       -- ** NFData Millisecond
       -- ** NFData Microsecond
       -- ** Buildable Attosecond
       -- ** Buildable Femtosecond
       -- ** Buildable Picosecond
       -- ** Buildable Nanosecond
       -- ** Buildable Millisecond
       -- ** Buildable Microsecond
       -- ** Buildable Second
       -- ** Buildable Minute
       -- ** Buildable Hour
       -- ** Buildable Day
       -- ** Buildable Week
       -- ** Buildable Fortnight

       -- ** Ether instances
       -- *** CanLog Ether.StateT
       -- *** HasLoggerName Ether.StateT
       ) where

import           Universum
import           Unsafe                         (unsafeInit, unsafeLast)

import           Control.Concurrent.ReadWriteLock (RWLock, acquireRead, acquireWrite,
                                                   releaseRead, releaseWrite)
import           Control.Lens                   (ALens', Getter, Getting, cloneLens, to)
import           Control.Monad.Base             (MonadBase)
import           Control.Monad.Morph            (MFunctor (..))
import           Control.Monad.STM              (retry)
import           Control.Monad.Trans.Class      (MonadTrans)
import           Control.Monad.Trans.Identity   (IdentityT (..))
import           Control.Monad.Trans.Lift.Local (LiftLocal (..))
import           Control.Monad.Trans.Resource   (MonadResource (..))
import           Data.Aeson                     (FromJSON (..), ToJSON (..))
import           Data.HashSet                   (fromMap)
import           Data.Tagged                    (Tagged (Tagged))
import           Data.Text.Buildable            (build)
import           Data.Time.Units                (Attosecond, Day, Femtosecond, Fortnight,
                                                 Hour, Microsecond, Millisecond, Minute,
                                                 Nanosecond, Picosecond, Second, Week,
                                                 toMicroseconds)
import           Data.Typeable                  (typeRep)
import qualified Ether
import qualified Formatting                     as F
import qualified Language.Haskell.TH.Syntax     as TH
import           Mockable                       (ChannelT, Counter, Distribution, Gauge,
                                                 MFunctor' (..), Mockable (..), Promise,
                                                 SharedAtomicT, SharedExclusiveT,
                                                 ThreadId)
import qualified Prelude
import           Serokell.Data.Memory.Units     (Byte, fromBytes, toBytes)
import           System.Wlog                    (CanLog, HasLoggerName (..),
                                                 LoggerNameBox (..))

----------------------------------------------------------------------------
-- Some
----------------------------------------------------------------------------

-- | Turn any constraint into an existential type! Example:
--
-- @
-- foo :: Some Show -> String
-- foo (Some s) = show s
-- @
data Some c where
    Some :: c a => a -> Some c

instance Show (Some Show) where
    show (Some s) = show s

-- | Like 'Some', but for @* -> *@ types – for instance, @Some1 Functor ()@
data Some1 c a where
    Some1 :: c f => f a -> Some1 c a

instance Functor (Some1 Functor) where
    fmap f (Some1 x) = Some1 (fmap f x)

-- | Apply a function requiring the constraint
applySome :: (forall a. c a => a -> r) -> (Some c -> r)
applySome f (Some x) = f x

-- | Turn a lens into something operating on 'Some'. Useful for many types
-- like 'HasDifficulty', 'IsHeader', etc.
liftLensSome :: (forall a. c a => ALens' a b)
             -> Lens' (Some c) b
liftLensSome l =
    \f (Some a) -> Some <$> cloneLens l f a

-- | Like 'liftLensSome', but for getters.
liftGetterSome :: (forall a. c a => Getting b a b) -> Getter (Some c) b
liftGetterSome l = \f (Some a) -> Some <$> to (view l) f a

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance TH.Lift Byte where
    lift x = let b = toBytes x in [|fromBytes b :: Byte|]

instance FromJSON Byte where
    parseJSON = fmap fromBytes . parseJSON

instance ToJSON Byte where
    toJSON = toJSON . toBytes

instance IsString s => MonadFail (Either s) where
    fail = Left . fromString

instance NFData Millisecond where
    rnf ms = rnf (toInteger ms)

instance NFData Microsecond where
    rnf ms = rnf (toInteger ms)

----------------------------------------------------------------------------
-- Orphan Buildable instances for time-units
----------------------------------------------------------------------------

instance Buildable Attosecond  where build = build @String . show
instance Buildable Femtosecond where build = build @String . show
instance Buildable Picosecond  where build = build @String . show
instance Buildable Nanosecond  where build = build @String . show
instance Buildable Millisecond where build = build @String . show
instance Buildable Second      where build = build @String . show
instance Buildable Minute      where build = build @String . show
instance Buildable Hour        where build = build @String . show
instance Buildable Day         where build = build @String . show
instance Buildable Week        where build = build @String . show
instance Buildable Fortnight   where build = build @String . show

-- | Special case. We don't want to print greek letter mu in console because
-- it breaks things sometimes.
instance Buildable Microsecond where
    build = build . (++ "mcs") . show . toMicroseconds

----------------------------------------------------------------------------
-- Ether instances
----------------------------------------------------------------------------

instance
    (Monad m, MonadTrans t, Monad (t m), CanLog m) =>
        CanLog (Ether.TaggedTrans tag t m)

instance (Monad m, CanLog m) => CanLog (IdentityT m)

instance
    (LiftLocal t, Monad m, HasLoggerName m) =>
        HasLoggerName (Ether.TaggedTrans tag t m)
  where
    getLoggerName = lift getLoggerName
    modifyLoggerName = liftLocal getLoggerName modifyLoggerName

instance
    (Monad m, HasLoggerName m) => HasLoggerName (IdentityT m)
  where
    getLoggerName = lift getLoggerName
    modifyLoggerName = liftLocal getLoggerName modifyLoggerName

deriving instance LiftLocal LoggerNameBox

instance {-# OVERLAPPABLE #-}
    (MonadResource m, MonadTrans t, Applicative (t m),
     MonadBase IO (t m), MonadIO (t m), MonadThrow (t m)) =>
        MonadResource (t m)
  where
    liftResourceT = lift . liftResourceT

instance {-# OVERLAPPABLE #-}
    (Monad m, MFunctor t) => MFunctor' t m n
  where
    hoist' = hoist

instance
    (Mockable d m, MFunctor' d (IdentityT m) m) =>
        Mockable d (IdentityT m)
  where
    liftMockable dmt = IdentityT $ liftMockable $ hoist' runIdentityT dmt

unTaggedTrans :: Ether.TaggedTrans tag t m a -> t m a
unTaggedTrans (Ether.TaggedTrans tma) = tma

instance
      (Mockable d (t m), Monad (t m),
       MFunctor' d (Ether.TaggedTrans tag t m) (t m)) =>
          Mockable d (Ether.TaggedTrans tag t m)
  where
    liftMockable dmt =
      Ether.TaggedTrans $ liftMockable $ hoist' unTaggedTrans dmt

type instance ThreadId (IdentityT m) = ThreadId m
type instance Promise (IdentityT m) = Promise m
type instance SharedAtomicT (IdentityT m) = SharedAtomicT m
type instance Counter (IdentityT m) = Counter m
type instance Distribution (IdentityT m) = Distribution m
type instance SharedExclusiveT (IdentityT m) = SharedExclusiveT m
type instance Gauge (IdentityT m) = Gauge m
type instance ChannelT (IdentityT m) = ChannelT m

type instance ThreadId (Ether.TaggedTrans tag t m) = ThreadId m
type instance Promise (Ether.TaggedTrans tag t m) = Promise m
type instance SharedAtomicT (Ether.TaggedTrans tag t m) = SharedAtomicT m
type instance Counter (Ether.TaggedTrans tag t m) = Counter m
type instance Distribution (Ether.TaggedTrans tag t m) = Distribution m
type instance SharedExclusiveT (Ether.TaggedTrans tag t m) = SharedExclusiveT m
type instance Gauge (Ether.TaggedTrans tag t m) = Gauge m
type instance ChannelT (Ether.TaggedTrans tag t m) = ChannelT m

----------------------------------------------------------------------------
-- Not instances
----------------------------------------------------------------------------

maybeThrow :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
maybeThrow e = maybe (throwM e) pure

-- | Fail or return result depending on what is stored in 'Either'.
eitherToFail :: (MonadFail m, ToString s) => Either s a -> m a
eitherToFail = either (fail . toString) pure

-- | Create HashSet from HashMap's keys
getKeys :: HashMap k v -> HashSet k
getKeys = fromMap . void

-- | Use some monadic action to evaluate priority of value and sort a
-- list of values based on this priority. The order is descending
-- because I need it.
sortWithMDesc :: (Monad m, Ord b) => (a -> m b) -> [a] -> m [a]
sortWithMDesc f = fmap (map fst . sortWith (Down . snd)) . mapM f'
  where
    f' x = (x, ) <$> f x

-- | Partial function which calls 'error' with meaningful message if
-- given 'Left' and returns some value if given 'Right'.
-- Intended usage is when you're sure that value must be right.
leftToPanic :: Buildable a => Text -> Either a b -> b
leftToPanic msgPrefix = either (error . mappend msgPrefix . pretty) identity

-- | Make a Reader or State computation work in an Ether transformer. Useful
-- to make lenses work with Ether.
ether :: trans m a -> Ether.TaggedTrans tag trans m a
ether = Ether.TaggedTrans

class PowerLift m n where
  powerLift :: m a -> n a

instance {-# OVERLAPPING #-} PowerLift m m where
  powerLift = identity

instance (MonadTrans t, PowerLift m n, Monad n) => PowerLift m (t n) where
  powerLift = lift . powerLift @m @n

instance (Typeable s, Buildable a) => Buildable (Tagged s a) where
    build tt@(Tagged v) = F.bprint ("Tagged " F.% F.shown F.% " " F.% F.build) ts v
      where
        ts = typeRep proxy
        proxy = (const Proxy :: Tagged s a -> Proxy s) tt

-- | This function performs checks at compile-time for different actions.
-- May slowdown implementation. To disable such checks (especially in benchmarks)
-- one should compile with: @stack build --flag cardano-sl-core:-asserts@
inAssertMode :: Applicative m => m a -> m ()
#ifdef ASSERTS_ON
inAssertMode x = x *> pure ()
#else
inAssertMode _ = pure ()
#endif
{-# INLINE inAssertMode #-}

----------------------------------------------------------------------------
-- Lenses
----------------------------------------------------------------------------

-- | Lens for the head of 'NonEmpty'.
--
-- We can't use '_head' because it doesn't work for 'NonEmpty':
-- <https://github.com/ekmett/lens/issues/636#issuecomment-213981096>.
-- Even if we could though, it wouldn't be a lens, only a traversal.
_neHead :: Lens' (NonEmpty a) a
_neHead f (x :| xs) = (:| xs) <$> f x

-- | Lens for the tail of 'NonEmpty'.
_neTail :: Lens' (NonEmpty a) [a]
_neTail f (x :| xs) = (x :|) <$> f xs

-- | Lens for the last element of 'NonEmpty'.
_neLast :: Lens' (NonEmpty a) a
_neLast f (x :| []) = (:| []) <$> f x
_neLast f (x :| xs) = (\y -> x :| unsafeInit xs ++ [y]) <$> f (unsafeLast xs)

----------------------------------------------------------------------------
-- Concurrency utilites (MVar/TVar/RWLock..)
----------------------------------------------------------------------------

clearMVar :: MonadIO m => MVar a -> m ()
clearMVar = void . tryTakeMVar

forcePutMVar :: MonadIO m => MVar a -> a -> m ()
forcePutMVar mvar val = do
    unlessM (tryPutMVar mvar val) $ do
        _ <- tryTakeMVar mvar
        forcePutMVar mvar val

-- | Block until value in MVar satisfies given predicate. When value
-- satisfies, it is returned.
readMVarConditional :: (MonadIO m) => (x -> Bool) -> MVar x -> m x
readMVarConditional predicate mvar = do
    rData <- readMVar mvar -- first we try to read for optimization only
    if predicate rData then pure rData
    else do
        tData <- takeMVar mvar         -- now take data
        if predicate tData then do     -- check again
            _ <- tryPutMVar mvar tData -- try to put taken value
            pure tData
        else
            readMVarConditional predicate mvar

-- | Read until value is equal to stored value comparing by some function.
readUntilEqualMVar
    :: (Eq a, MonadIO m)
    => (x -> a) -> MVar x -> a -> m x
readUntilEqualMVar f mvar expVal = readMVarConditional ((expVal ==) . f) mvar

-- | Block until value in TVar satisfies given predicate. When value
-- satisfies, it is returned.
readTVarConditional :: (MonadIO m) => (x -> Bool) -> TVar x -> m x
readTVarConditional predicate tvar = atomically $ do
    res <- readTVar tvar
    if predicate res then pure res
    else retry

-- | Read until value is equal to stored value comparing by some function.
readUntilEqualTVar
    :: (Eq a, MonadIO m)
    => (x -> a) -> TVar x -> a -> m x
readUntilEqualTVar f tvar expVal = readTVarConditional ((expVal ==) . f) tvar

withReadLifted :: (MonadIO m, MonadMask m) => RWLock -> m a -> m a
withReadLifted l = bracket_ (liftIO $ acquireRead l) (liftIO $ releaseRead l)

withWriteLifted :: (MonadIO m, MonadMask m) => RWLock -> m a -> m a
withWriteLifted l = bracket_ (liftIO $ acquireWrite l) (liftIO $ releaseWrite l)
