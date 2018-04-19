{-# LANGUAGE TypeFamilies #-}

-- | Wrapper for the modifier pattern which is used throughout the code.

module Pos.Util.Modifier
       ( MapModifier
       , lookupM
       , lookup
       , filter
       , filterWithKey
       , keysM
       , keys
       , valuesM
       , values
       , toListM
       , toList
       , insertions
       , insertionsMap
       , deletions
       , deletionsSet

       , insert
       , delete
       , alter
       , KeyState (..)

       , mapMaybeM
       , mapMaybe
       , modifyHashMap
       , modifyMap
       , foldlMapModWKey'
       , fromHashMap
       , toHashMap
       ) where

import           Universum hiding (filter, mapMaybe, toList)
import qualified Universum

import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text.Buildable
import           Formatting (bprint, (%))
import           Serokell.Util (listJson, pairF)
import           Test.QuickCheck (Arbitrary)
import           Test.QuickCheck.Instances ()

import           Pos.Util.Util (getKeys)

-- | 'MapModifier' is a type which collects modifications (insertions
-- and deletions) of something map-like.
newtype MapModifier k v = MapModifier
    { getMapModifier :: HashMap k (Maybe v)
    } deriving (Eq, Show)

instance Functor (MapModifier k) where
    fmap f (MapModifier m) = MapModifier (f <<$>> m)

deriving instance (Eq k, Hashable k, Arbitrary k, Arbitrary v) =>
    Arbitrary (MapModifier k v)

instance (Eq k, Hashable k) =>
         Monoid (MapModifier k v) where
    mempty = MapModifier mempty
    mappend m1 (MapModifier m2) = HM.foldlWithKey' step m1 m2
      where
        step m k Nothing  = delete k m
        step m k (Just v) = insert k v m

instance (Eq k, Hashable k) => Semigroup (MapModifier k v)

instance (Buildable k, Buildable v) => Buildable (MapModifier k v) where
    build mm =
      bprint ("MapModifier { deletions "%listJson%", insertions: "%listJson%"}")
      (deletions mm) (map (bprint pairF) (insertions mm))

-- | Perform monadic lookup taking 'MapModifier' into account.
lookupM
    :: (Applicative m, Eq k, Hashable k)
    => (k -> m (Maybe v)) -> k -> MapModifier k v -> m (Maybe v)
lookupM getter k (MapModifier m) =
    case HM.lookup k m of
        Nothing      -> getter k
        Just Nothing -> pure Nothing
        Just justX   -> pure justX

-- | Perform lookup taking 'MapModifier' into account.
lookup
    :: (Eq k, Hashable k)
    => (k -> Maybe v) -> k -> MapModifier k v -> Maybe v
lookup getter k = runIdentity . lookupM (Identity . getter) k

filter :: (Eq k, Hashable k) => (Maybe v -> Bool) -> MapModifier k v -> MapModifier k v
filter fil = MapModifier . HM.filter fil . getMapModifier

filterWithKey :: (Eq k, Hashable k) => (k -> Maybe v -> Bool) -> MapModifier k v -> MapModifier k v
filterWithKey fil = MapModifier . HM.filterWithKey fil . getMapModifier

-- | Get keys of something map-like in Functor context taking
-- 'MapModifier' into account.
keysM
    :: (Functor m, Eq k, Hashable k)
    => m [k] -> MapModifier k v -> m [k]
keysM getter (MapModifier m) = keysDo <$> getter
  where
    keysDo ks =
        HM.keys (HM.filter isJust m) <> Universum.filter (not . flip HM.member m) ks

-- | Get keys of something map-like taking 'MapModifier' into account.
keys
    :: (Eq k, Hashable k)
    => [k] -> MapModifier k v -> [k]
keys getter = runIdentity . keysM (Identity getter)

-- | Get values of something map-like in Functor context taking
-- 'MapModifier' into account.
valuesM
    :: (Functor m, Eq k, Hashable k)
    => m [(k, v)] -> MapModifier k v -> m [v]
valuesM getter (MapModifier m) = valuesDo <$> getter
  where
    valuesDo vs =
        HM.elems (HM.mapMaybe identity m) <>
        map snd (Universum.filter (not . flip HM.member m . fst) vs)

-- | Get values of something map-like taking 'MapModifier' into account.
values
    :: (Eq k, Hashable k)
    => [(k, v)] -> MapModifier k v -> [v]
values getter = runIdentity . valuesM (Identity getter)

-- | Get contents of something map-like in Functor context taking
-- 'MapModifier' into account.
toListM
    :: (Functor m, Eq k, Hashable k)
    => m [(k, v)] -> MapModifier k v -> m [(k, v)]
toListM getter mm@(MapModifier m) = toListDo <$> getter
  where
    toListDo kvs = insertions mm <> Universum.filter (not . flip HM.member m . fst) kvs

-- | Get contents of something map-like taking 'MapModifier' into account.
toList
    :: (Eq k, Hashable k)
    => [(k, v)] -> MapModifier k v -> [(k, v)]
toList getter = runIdentity . toListM (Identity getter)

-- | Get all insertions into 'MapModifier' as a 'HashMap'.
insertionsMap :: MapModifier k v -> HashMap k v
insertionsMap = HM.mapMaybe identity . getMapModifier

-- | Get all insertions into 'MapModifier' as a list.
insertions :: MapModifier k v -> [(k, v)]
insertions = HM.toList . insertionsMap

-- | Get all deletions from 'MapModifier' as a 'HashSet'.
deletionsSet :: MapModifier k v -> HashSet k
deletionsSet = getKeys . HM.filter isNothing . getMapModifier

-- | Get all deletions from 'MapModifier'.
deletions :: MapModifier k v -> [k]
deletions = HM.keys . HM.filter isNothing . getMapModifier

-- | Insert something into 'MapModifier'. Effect of it is like
-- inserting something into underlying container (which replaces
-- existing value if it exists).
insert
    :: (Eq k, Hashable k)
    => k -> v -> MapModifier k v -> MapModifier k v
insert k v (MapModifier m) = MapModifier $ HM.insert k (Just v) m

-- | Delete something into 'MapModifier'. Nothing is actually deleted
-- from 'MapModifier' itself. Effect is akin to deleting value from
-- underlying container.
delete
    :: (Eq k, Hashable k)
    => k -> MapModifier k v -> MapModifier k v
delete k (MapModifier m) = MapModifier $ HM.insert k Nothing m

data KeyState v
    = KeyNotFound
    | KeyDeleted
    | KeyInserted v

alter
    :: (Eq k, Hashable k)
    => (KeyState v -> KeyState v)
    -> k
    -> MapModifier k v
    -> MapModifier k v
alter f key (MapModifier mm) = MapModifier $ HM.alter transformedF key mm
  where
    toMaybe KeyNotFound     = Nothing
    toMaybe KeyDeleted      = Just Nothing
    toMaybe (KeyInserted v) = Just $ Just v

    transformedF Nothing         = toMaybe $ f KeyNotFound
    transformedF (Just Nothing)  = toMaybe $ f KeyDeleted
    transformedF (Just (Just v)) = toMaybe $ f (KeyInserted v)

-- | Transform this modifier in Functor context by applying a function to every
-- insertion and retaining only some of them. Underlying map should be already
-- transformed.
mapMaybeM
    :: (Functor m, Eq k, Hashable k)
    => m [(k, v2)] -> (v1 -> Maybe v2) -> MapModifier k v1 -> m [(k, v2)]
mapMaybeM getter f mm@(MapModifier m) = mapMaybeDo <$> getter
  where
    mapMaybeDo kvs =
        Universum.mapMaybe (\(k, v) -> (k, ) <$> f v) (insertions mm) <>
        Universum.filter (not . flip HM.member m . fst) kvs

-- | Transform this modifier by applying a function to every insertion and retaining
-- only some of them. Underlying map should be already transformed.
mapMaybe
    :: (Eq k, Hashable k)
    => [(k, v2)] -> (v1 -> Maybe v2) -> MapModifier k v1 -> [(k, v2)]
mapMaybe getter f = runIdentity . mapMaybeM (Identity getter) f

-- | Applies a map modifier to a hashmap, returning the result
modifyHashMap :: (Eq k, Hashable k) => MapModifier k v -> HashMap k v -> HashMap k v
modifyHashMap pm hm =
    foldl' (flip (uncurry HM.insert)) (foldl' (flip HM.delete) hm deletes) inserts
  where
    inserts = insertions pm
    deletes = deletions pm

modifyMap :: Ord k => MapModifier k v -> Map k v -> Map k v
modifyMap pm hm =
    foldl' (flip (uncurry M.insert)) (foldl' (flip M.delete) hm deletes) inserts
  where
    inserts = insertions pm
    deletes = deletions pm

foldlMapModWKey'
    :: (Eq k, Hashable k)
    => (a -> k -> Maybe v -> a)
    -> a
    -> MapModifier k v
    -> a
foldlMapModWKey' f acc = HM.foldlWithKey' f acc . getMapModifier

toHashMap :: MapModifier k v -> HashMap k (Maybe v)
toHashMap = getMapModifier

fromHashMap :: HashMap k (Maybe v) -> MapModifier k v
fromHashMap = MapModifier
