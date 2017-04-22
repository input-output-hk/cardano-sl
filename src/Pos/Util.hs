{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Miscellaneous unclassified utility functions.

module Pos.Util
       (
         module Pos.Util.Util
       -- * Stuff for testing and benchmarking
       , module Pos.Util.Arbitrary
       , module Pos.Util.TimeLimit

       -- * Various
       , mappendPair
       , mconcatPair
       , (<//>)
       , readerToState
       , eitherPanic
       , inAssertMode
       , diffDoubleMap
       , maybeThrow'

       -- * NonEmpty
       , neZipWith3
       , spanSafe

       -- * Lenses
       , makeLensesData
       , _neHead
       , _neTail
       , _neLast

       -- * LRU
       , clearLRU

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
       -- ** MonadFail ParsecT
       -- ** MonadFail Dialog
       -- ** MonadFail Transfer
       -- ** MonadFail TimedIO
       -- ** MonadFail ResponseT
       -- ** MonadFail LoggerNameBox
       ) where

import           Universum                        hiding (bracket, finally)

import           Control.Concurrent.ReadWriteLock (RWLock, acquireRead, acquireWrite,
                                                   releaseRead, releaseWrite)
import           Control.Lens                     (lensRules)
import           Control.Lens.Internal.FieldTH    (makeFieldOpticsForDec)
import qualified Control.Monad                    as Monad (fail)
import           Control.Monad.STM                (retry)
import           Control.Monad.Trans.Resource     (ResourceT)
import qualified Data.Cache.LRU                   as LRU
import           Data.Hashable                    (Hashable)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (span, zipWith3)
import qualified Data.Text                        as T
import qualified Language.Haskell.TH              as TH
import           Mockable                         (Mockable, Throw, throw)
import           Serokell.Util                    (VerificationRes (..))
import           System.Wlog                      (LoggerNameBox (..))
import           Text.Parsec                      (ParsecT)
import           Unsafe                           (unsafeInit, unsafeLast)
-- SafeCopy instance for HashMap
import           Serokell.AcidState               ()

import           Pos.Util.Arbitrary
import           Pos.Util.TimeLimit
import           Pos.Util.Undefined               ()
import           Pos.Util.Util

-- | Specialized version of 'mappend' for restricted to pair type.
mappendPair :: (Monoid a, Monoid b) => (a, b) -> (a, b) -> (a, b)
mappendPair = mappend

-- | Specialized version of 'mconcat' (or 'Data.Foldable.fold')
-- for restricting type to list of pairs.
mconcatPair :: (Monoid a, Monoid b) => [(a, b)] -> (a, b)
mconcatPair = mconcat

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

-- | A helper for simple error handling in executables
eitherPanic :: Show a => Text -> Either a b -> b
eitherPanic msgPrefix = either (error . (msgPrefix <>) . show) identity

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

maybeThrow' :: (Mockable Throw m, Exception e) => e -> Maybe a -> m a
maybeThrow' e = maybe (throw e) pure

----------------------------------------------------------------------------
-- NonEmpty
----------------------------------------------------------------------------

neZipWith3 :: (x -> y -> z -> q) -> NonEmpty x -> NonEmpty y -> NonEmpty z -> NonEmpty q
neZipWith3 f (x :| xs) (y :| ys) (z :| zs) = f x y z :| zipWith3 f xs ys zs

-- | Makes a span on the list, considering tail only. Predicate has
-- list head as first argument. Used to take non-null prefix that
-- depends on the first element.
spanSafe :: (a -> a -> Bool) -> NonEmpty a -> (NonEmpty a, [a])
spanSafe p (x:|xs) = let (a,b) = span (p x) xs in (x:|a,b)

----------------------------------------------------------------------------
-- Lens utils
----------------------------------------------------------------------------

-- | Make lenses for a data family instance.
makeLensesData :: TH.Name -> TH.Name -> TH.DecsQ
makeLensesData familyName typeParamName = do
    info <- TH.reify familyName
    ins <- case info of
        TH.FamilyI _ ins -> return ins
        _                -> fail "makeLensesIndexed: expected data family name"
    typeParamInfo <- TH.reify typeParamName
    typeParam <- case typeParamInfo of
        TH.TyConI dec -> decToType dec
        _             -> fail "makeLensesIndexed: expected a type"
    let mbInsDec = find ((== Just typeParam) . getTypeParam) ins
    case mbInsDec of
        Nothing -> fail ("makeLensesIndexed: an instance for " ++
                         TH.nameBase typeParamName ++ " not found")
        Just insDec -> makeFieldOpticsForDec lensRules insDec
  where
    getTypeParam (TH.NewtypeInstD _ _ [t] _ _ _) = Just t
    getTypeParam (TH.DataInstD    _ _ [t] _ _ _) = Just t
    getTypeParam _                               = Nothing

    decToType (TH.DataD    _ n _ _ _ _) = return (TH.ConT n)
    decToType (TH.NewtypeD _ n _ _ _ _) = return (TH.ConT n)
    decToType other                     =
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

----------------------------------------------------------------------------
-- LRU cache
----------------------------------------------------------------------------

-- | Remove all items from LRU, retaining maxSize property.
clearLRU :: Ord k => LRU.LRU k v -> LRU.LRU k v
clearLRU = LRU.newLRU . LRU.maxSize

----------------------------------------------------------------------------
-- Deserialized wrapper
----------------------------------------------------------------------------

eitherToVerRes :: Either Text a -> VerificationRes
eitherToVerRes (Left errors) = if T.null errors then VerFailure []
                               else VerFailure $ T.split (==';') errors
eitherToVerRes (Right _ )    = VerSuccess


instance MonadFail (ParsecT s u m) where
    fail = Monad.fail

deriving instance MonadFail m => MonadFail (LoggerNameBox m)

instance MonadFail m => MonadFail (ResourceT m) where
    fail = lift . fail

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
        tData <- takeMVar mvar -- now take data
        if predicate tData then do -- check again
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
