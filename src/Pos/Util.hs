{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Miscellaneous unclassified utility functions.

module Pos.Util
       (
       -- * Stuff for testing and benchmarking
         module Pos.Util.Arbitrary
       , module Pos.Util.TimeLimit

       -- * Various
       , mappendPair
       , mconcatPair
       , (<//>)
       , readerToState
       , eitherPanic
       , inAssertMode
       , diffDoubleMap
       , getKeys
       , maybeThrow
       , maybeThrow'

       -- * NonEmpty
       , NE
       , neZipWith3

       -- * Chronological sequences
       , NewestFirst(..)
       , OldestFirst(..)
       , toNewestFirst
       , toOldestFirst

       -- * Lenses
       , makeLensesData
       , _neHead
       , _neTail
       , _neLast

       -- * LRU
       , clearLRU

       , spanSafe
       , eitherToVerRes

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
       -- ** MonadFail ParsecT
       -- ** MonadFail Dialog
       -- ** MonadFail Transfer
       -- ** MonadFail TimedIO
       -- ** MonadFail ResponseT
       -- ** MonadFail LoggerNameBox
       ) where

import           Control.Arrow                    ((***))
import           Control.Concurrent.ReadWriteLock (RWLock, acquireRead, acquireWrite,
                                                   releaseRead, releaseWrite)
import           Control.Concurrent.STM.TVar      (TVar, readTVar)
import           Control.Lens                     (Each (..), lensRules, makeWrapped,
                                                   _Wrapped)
import           Control.Lens.Internal.FieldTH    (makeFieldOpticsForDec)
import qualified Control.Monad                    as Monad (fail)
import           Control.Monad.STM                (retry)
import           Control.Monad.Trans.Resource     (ResourceT)
import           Data.Aeson                       (FromJSON (..), ToJSON (..))
import           Data.Binary                      (Binary)
import qualified Data.Cache.LRU                   as LRU
import           Data.Hashable                    (Hashable)
import qualified Data.HashMap.Strict              as HM
import           Data.HashSet                     (fromMap)
import           Data.List                        (span, zipWith3)
import qualified Data.List.NonEmpty               as NE
import           Data.SafeCopy                    (SafeCopy (..), base, contain,
                                                   deriveSafeCopySimple, safeGet, safePut)
import qualified Data.Text                        as T
import           Formatting                       (sformat, stext, (%))
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax       (Lift)
import qualified Language.Haskell.TH.Syntax
import           Mockable                         (Mockable, Throw, throw)
import           Prelude                          (read)
import           Serokell.Data.Memory.Units       (Byte, fromBytes, toBytes)
import           Serokell.Util                    (VerificationRes (..))
import           System.Wlog                      (LoggerNameBox (..))
import           Test.QuickCheck                  (Arbitrary)
import           Text.Parsec                      (ParsecT, digit)
import           Text.Parsec.Text                 (Parser)
import           Universum                        hiding (Async, async, bracket, cancel,
                                                   finally, waitAny)
import           Unsafe                           (unsafeInit, unsafeLast)
-- SafeCopy instance for HashMap
import           Serokell.AcidState               ()

import           Pos.Binary.Class                 (Bi)
import           Pos.Util.Arbitrary
import           Pos.Util.NotImplemented          ()
import           Pos.Util.TimeLimit

mappendPair :: (Monoid a, Monoid b) => (a, b) -> (a, b) -> (a, b)
mappendPair = (uncurry (***)) . (mappend *** mappend)

mconcatPair :: (Monoid a, Monoid b) => [(a, b)] -> (a, b)
mconcatPair = foldr mappendPair (mempty, mempty)

-- | Concatenates two url part using regular slash '/'.
-- E.g. @"./dir/" <//> "/file" = "./dir/file"@.
(<//>) :: String -> String -> String
(<//>) lhs rhs = lhs' ++ "/" ++ rhs'
  where
    isSlash = (== '/')
    lhs' = reverse $ dropWhile isSlash $ reverse lhs
    rhs' = dropWhile isSlash rhs

-- | Convert (Reader s) to any (MonadState s)
readerToState
    :: MonadState s m
    => Reader s a -> m a
readerToState = gets . runReader

deriveSafeCopySimple 0 'base ''VerificationRes

-- | A helper for simple error handling in executables
eitherPanic :: Show a => Text -> Either a b -> b
eitherPanic msgPrefix = either (panic . (msgPrefix <>) . show) identity

-- | This function performs checks at compile-time for different actions.
-- May slowdown implementation. To disable such checks (especially in benchmarks)
-- one should compile with: @stack build --flag cardano-sl:-asserts@
inAssertMode :: Applicative m => m a -> m ()
#ifdef ASSERTS_ON
inAssertMode x = x *> pure ()
#else
inAssertMode _ = pure ()
#endif
{-# INLINE inAssertMode #-}

-- | Remove elements which are in 'b' from 'a'
diffDoubleMap
    :: forall k1 k2 v.
       (Eq k1, Eq k2, Hashable k1, Hashable k2)
    => HashMap k1 (HashMap k2 v)
    -> HashMap k1 (HashMap k2 v)
    -> HashMap k1 (HashMap k2 v)
diffDoubleMap a b = HM.foldlWithKey' go mempty a
  where
    go :: HashMap k1 (HashMap k2 v)
       -> k1
       -> HashMap k2 v
       -> HashMap k1 (HashMap k2 v)
    go res extKey internalMap =
        case HM.lookup extKey b of
            Nothing -> HM.insert extKey internalMap res
            Just internalMapB ->
                let diff = internalMap `HM.difference` internalMapB
                in if null diff
                       then res
                       else HM.insert extKey diff res

maybeThrow :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
maybeThrow e = maybe (throwM e) pure

maybeThrow' :: (Mockable Throw m, Exception e) => e -> Maybe a -> m a
maybeThrow' e = maybe (throw e) pure

----------------------------------------------------------------------------
-- NonEmpty
----------------------------------------------------------------------------

type NE = NonEmpty

neZipWith3 :: (x -> y -> z -> q) -> NonEmpty x -> NonEmpty y -> NonEmpty z -> NonEmpty q
neZipWith3 f (x :| xs) (y :| ys) (z :| zs) = f x y z :| zipWith3 f xs ys zs

-- | Makes a span on the list, considering tail only. Predicate has
-- list head as first argument. Used to take non-null prefix that
-- depends on the first element.
spanSafe :: (a -> a -> Bool) -> NonEmpty a -> (NonEmpty a, [a])
spanSafe p (x:|xs) = let (a,b) = span (p x) xs in (x:|a,b)

----------------------------------------------------------------------------
-- Chronological sequences
----------------------------------------------------------------------------

newtype NewestFirst f a = NewestFirst {getNewestFirst :: f a}
  deriving (Eq, Ord, Show,
            Functor, Foldable, Traversable,
            Container, NontrivialContainer,
            Binary, Bi,
            Arbitrary)
newtype OldestFirst f a = OldestFirst {getOldestFirst :: f a}
  deriving (Eq, Ord, Show,
            Functor, Foldable, Traversable,
            Container, NontrivialContainer,
            Binary, Bi,
            Arbitrary)

makeWrapped ''NewestFirst
makeWrapped ''OldestFirst

instance Each (f a) (f b) a b =>
         Each (NewestFirst f a) (NewestFirst f b) a b where
    each = _Wrapped . each
instance Each (f a) (f b) a b =>
         Each (OldestFirst f a) (OldestFirst f b) a b where
    each = _Wrapped . each

instance One (f a) => One (NewestFirst f a) where
    type OneItem (NewestFirst f a) = OneItem (f a)
    one = NewestFirst . one
instance One (f a) => One (OldestFirst f a) where
    type OneItem (OldestFirst f a) = OneItem (f a)
    one = OldestFirst . one

class Chrono f where
    toNewestFirst :: OldestFirst f a -> NewestFirst f a
    toOldestFirst :: NewestFirst f a -> OldestFirst f a

instance Chrono [] where
    toNewestFirst = NewestFirst . reverse . getOldestFirst
    toOldestFirst = OldestFirst . reverse . getNewestFirst

instance Chrono NonEmpty where
    toNewestFirst = NewestFirst . NE.reverse . getOldestFirst
    toOldestFirst = OldestFirst . NE.reverse . getNewestFirst

----------------------------------------------------------------------------
-- Lens utils
----------------------------------------------------------------------------

-- | Make lenses for a data family instance.
makeLensesData :: Name -> Name -> DecsQ
makeLensesData familyName typeParamName = do
    info <- reify familyName
    ins <- case info of
        FamilyI _ ins -> return ins
        _             -> fail "makeLensesIndexed: expected data family name"
    typeParamInfo <- reify typeParamName
    typeParam <- case typeParamInfo of
        TyConI dec -> decToType dec
        _          -> fail "makeLensesIndexed: expected a type"
    let mbInsDec = find ((== Just typeParam) . getTypeParam) ins
    case mbInsDec of
        Nothing -> fail ("makeLensesIndexed: an instance for " ++
                         nameBase typeParamName ++ " not found")
        Just insDec -> makeFieldOpticsForDec lensRules insDec
  where
    getTypeParam (NewtypeInstD _ _ [t] _ _ _) = Just t
    getTypeParam (DataInstD    _ _ [t] _ _ _) = Just t
    getTypeParam _                            = Nothing

    decToType (DataD    _ n _ _ _ _) = return (ConT n)
    decToType (NewtypeD _ n _ _ _ _) = return (ConT n)
    decToType other                  =
        fail ("makeLensesIndexed: decToType failed on: " ++ show other)

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

instance Lift Byte where
    lift x = let b = toBytes x in [|fromBytes b :: Byte|]

instance FromJSON Byte where
    parseJSON = fmap fromBytes . parseJSON

instance ToJSON Byte where
    toJSON = toJSON . toBytes

----------------------------------------------------------------------------
-- LRU cache
----------------------------------------------------------------------------

-- | Remove all items from LRU, retaining maxSize property.
clearLRU :: Ord k => LRU.LRU k v -> LRU.LRU k v
clearLRU = LRU.newLRU . LRU.maxSize

-- [SRK-51]: Probably this instance should be in @safecopy@ library
instance (Ord k, SafeCopy k, SafeCopy v) =>
         SafeCopy (LRU.LRU k v) where
    getCopy = contain $ LRU.fromList <$> safeGet <*> safeGet
    putCopy lru =
        contain $
        do safePut $ LRU.maxSize lru
           safePut $ LRU.toList lru
    errorTypeName _ = "LRU"

-- | Create HashSet from HashMap's keys
getKeys :: HashMap k v -> HashSet k
getKeys = fromMap . void

----------------------------------------------------------------------------
-- Deserialized wrapper
----------------------------------------------------------------------------

eitherToVerRes :: Either Text a -> VerificationRes
eitherToVerRes (Left errors) = if T.null errors then VerFailure []
                               else VerFailure $ T.split (==';') errors
eitherToVerRes (Right _ )    = VerSuccess


instance IsString s => MonadFail (Either s) where
    fail = Left . fromString

instance MonadFail (ParsecT s u m) where
    fail = Monad.fail

deriving instance MonadFail m => MonadFail (LoggerNameBox m)

instance MonadFail m => MonadFail (ResourceT m) where
    fail = lift . fail

----------------------------------------------------------------------------
-- Concurrency utilites (MVar/TVar/RWLock..)
----------------------------------------------------------------------------

clearMVar :: MonadIO m => MVar a -> m ()
clearMVar = liftIO . void . tryTakeMVar

forcePutMVar :: MonadIO m => MVar a -> a -> m ()
forcePutMVar mvar val = do
    res <- liftIO $ tryPutMVar mvar val
    unless res $ do
        _ <- liftIO $ tryTakeMVar mvar
        forcePutMVar mvar val

-- | Block until value in MVar satisfies given predicate. When value
-- satisfies, it is returned.
readMVarConditional :: (MonadIO m) => (x -> Bool) -> MVar x -> m x
readMVarConditional predicate mvar = do
    rData <- liftIO . readMVar $ mvar -- first we try to read for optimization only
    if predicate rData then pure rData
    else do
        tData <- liftIO . takeMVar $ mvar -- now take data
        if predicate tData then do -- check again
            _ <- liftIO $ tryPutMVar mvar tData -- try to put taken value
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
