{-# LANGUAGE TypeFamilies #-}

-- | Wrapper for modifier pattern which is used intensively in code

module Pos.Util.Modifier
       ( MapModifier
       , lookupM
       , lookup
       , keysM
       , keys
       , valuesM
       , values
       , toListM
       , toList
       , insertions
       , deletions

       , insert
       , delete

       , mapMaybeM
       ) where

import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HM
import           Universum           hiding (toList)

-- | 'MapModifier' is a type which collects modifications (insertions
-- and deletions) of something map-like.
newtype MapModifier k v = MapModifier
    { getMapModifier :: HashMap k (Maybe v)
    } deriving (Show)

instance (Eq k, Hashable k) =>
         Monoid (MapModifier k v) where
    mempty = MapModifier mempty
    mappend m1 (MapModifier m2) = HM.foldlWithKey' step m1 m2
      where
        step m k Nothing  = delete k m
        step m k (Just v) = insert k v m

instance (Eq k, Hashable k) => Semigroup (MapModifier k v)

-- | Perform monadic lookup taking 'MapModifier' into account.
lookupM
    :: (Applicative m, Eq k, Hashable k)
    => (k -> m (Maybe v)) -> k -> MapModifier k v -> m (Maybe v)
lookupM getter k (MapModifier m) =
    case HM.lookup k m of
        Nothing       -> getter k
        Just Nothing  -> pure Nothing
        Just (Just x) -> purer x

-- | Perform lookup taking 'MapModifier' into account.
lookup
    :: (Eq k, Hashable k)
    => (k -> Maybe v) -> k -> MapModifier k v -> Maybe v
lookup getter k = runIdentity . lookupM (Identity . getter) k

-- | Get keys of something map-like in Functor context taking
-- 'MapModifier' into account.
keysM
    :: (Functor m, Eq k, Hashable k)
    => m [k] -> MapModifier k v -> m [k]
keysM getter (MapModifier m) = keysDo <$> getter
  where
    keysDo ks =
        HM.keys (HM.filter isJust m) <> filter (not . flip HM.member m) ks

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
        map snd (filter (not . flip HM.member m . fst) vs)

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
    toListDo kvs = insertions mm <> filter (not . flip HM.member m . fst) kvs

-- | Get contents of something map-like taking 'MapModifier' into account.
toList
    :: (Eq k, Hashable k)
    => [(k, v)] -> MapModifier k v -> [(k, v)]
toList getter = runIdentity . toListM (Identity getter)

-- | Get all insertions into 'MapModifier'.
insertions :: MapModifier k v -> [(k, v)]
insertions = HM.toList . HM.mapMaybe identity . getMapModifier

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

-- | Transform this modifer by applying a function to every insertion
-- and retaining only some of them. Underlying map should be already
-- transformed.
mapMaybeM
    :: (Functor m, Eq k, Hashable k)
    => m [(k, v2)] -> (v1 -> Maybe v2) -> MapModifier k v1 -> m [(k, v2)]
mapMaybeM getter f mm@(MapModifier m) = mapMaybeDo <$> getter
  where
    mapMaybeDo kvs =
        mapMaybe (\(k, v) -> (k, ) <$> f v) (insertions mm) <>
        filter (not . flip HM.member m . fst) kvs
