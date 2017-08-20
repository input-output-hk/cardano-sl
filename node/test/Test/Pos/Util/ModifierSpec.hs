{-# LANGUAGE GADTs #-}

-- | Specification for Pos.Util.Modifier (from 'core')

module Test.Pos.Util.ModifierSpec
       ( spec
       ) where

import           Universum

import qualified Data.HashMap.Strict   as HM
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Property, (==>))

import qualified Pos.Util.Modifier     as Core
import           Test.Pos.Util         (formsMonoid)

spec :: Spec
spec = describe "MapModifier" $ do
    describe "Monoid instance" $ do
        prop
            "Map modifiers form a monoid instance with the custom binary operation"
            (mapModifierFormsMonoid @Text @Text)
    describe "Operations on map" $ do
        describe "insert" $ do
            prop
                "inserting a value and then looking it up returns the inserted value"
                (insertThenLookup @Text @Text)
        describe "delete" $ do
            prop
                "a key inserted in the underlying hashmap and then deleted from the map\
                \ modifier will not be present in the hashmap"
                (insertInHMDeleteThenLookup @Text @Text)
            prop
                "a key inserted in a map modifier and then deleted from the underlying\
                \ hashmap will be present in the map modifier with its value"
                (insertInMMDeleteThenLookup @Text @Text)
        describe "keys" $ do
            prop
                "getting the keys from a map modifier does not eliminate any from the\
                \ produced list unless they are in the argument list and are a key in the\
                \ map modifier simultanously"
                (allKeysAreInMap @Text @Text)
        describe "values" $ do
            prop
                "returns all values from underlying storage except deleted values,\
                \ overrides values according to insertions into MapModifier and it also\
                \ returns new values inserted into MapModifier"
                (allValsAreInMap @Text @Text)
        describe "toList" $ do
            prop
                "getting the contents of the map modifier as an association list will\
                \ keep all of the keys and will include the pairs in the argument list\
                \ whose key is not present in the modifier"
                (allPairsAreInAssocList @Text @Text)
        describe "insertionsMap" $ do
            prop
                "the created hashmap removes all the deleted keys from the 'MapModifier'\
                \ and keeps all the inserted keys"
                (mapModifierToHashMap @Text @Text)
        describe "insertions" $ do
            prop
                "the created list removes all the deleted keys from the used\
                \ 'MapModifier' and keeps all the inserted keys in the association list"
                (mapModifierToList @Text @Text)
        describe "deletions" $ do
            prop
                "the created list of keys removes all the inserted values and keeps those\
                \ which have been deleted"
                (mapModifierToDeletionList @Text @Text)
        describe "mapMaybe" $ do
            prop
                "the created list of keys removes all the deleted values and keeps the\
                \ pairs from the input list whose keys are not in the modifier"
                (allPairsAreInMaybeList @Text @Text @Text Just)
        describe "modifyHashMap" $ do
            prop "modifying a hashmap with a map modifier removes the keys that were\
                 \ deleted and keeps those which were inserted, along with their values"
                 (hashMapIsModified @Text @Text)


mapModifierFormsMonoid
    :: (Show k, Show v, Eq k, Eq v, Hashable k)
    => Core.MapModifier k v
    -> Core.MapModifier k v
    -> Core.MapModifier k v
    -> Property
mapModifierFormsMonoid = formsMonoid

-- | The 'v1 /= v2' implication here emphasizes the fact that the values inserted into
-- both the hashmap and the map modifier don't have to be equal for this property to hold.
-- In fact, them being equal is a case not worth concern because the inner
-- 'flip HM.lookup newHMap' is going to make the property work in any case.
insertThenLookup
    :: (Show k, Show v, Eq k, Eq v, Hashable k)
    => k
    -> v
    -> v
    -> HM.HashMap k v
    -> Core.MapModifier k v
    -> Property
insertThenLookup k v1 v2 hm mapMod =
    let newMap = Core.insert k v1 mapMod
        newHMap = HM.insert k v2 hm
    in v1 /= v2 ==> (== Just v1) $ Core.lookup (flip HM.lookup newHMap) k newMap

insertInHMDeleteThenLookup
    :: (Show k, Show v, Eq k, Eq v, Hashable k)
    => HashMap k v
    -> k
    -> v
    -> Core.MapModifier k v
    -> Bool
insertInHMDeleteThenLookup m k v mapMod =
    let newMap = HM.insert k v m
        newMapMod = Core.delete k mapMod
    in isNothing $ Core.lookup (flip HM.lookup newMap) k newMapMod

insertInMMDeleteThenLookup
    :: (Show k, Show v, Eq k, Eq v, Hashable k)
    => HashMap k v
    -> k
    -> v
    -> Core.MapModifier k v
    -> Bool
insertInMMDeleteThenLookup m k v mapMod =
    let newMapMod = Core.insert k v mapMod
        newMap = HM.delete k m
    in Just v == Core.lookup (flip HM.lookup newMap) k newMapMod

allKeysAreInMap
    :: (Ord k, Show k, Show v, Eq k, Eq v, Hashable k)
    => HM.HashMap k v
    -> Core.MapModifier k v
    -> Bool
allKeysAreInMap hm mapMod =
    let keys = Core.keys (HM.keys hm) mapMod
    in all (\k -> (isJust $ Core.lookup (flip HM.lookup hm) k mapMod)) keys

allValsAreInMap
    :: (Ord k, Show k, Show v, Eq k, Eq v, Hashable k)
    => [(k, v)]
    -> Core.MapModifier k v
    -> Bool
allValsAreInMap kvs mapMod =
    let vals = Core.values kvs mapMod
        ins = Core.insertions mapMod
    in all (\v -> (v `elem` (fmap snd ins) || v `elem` fmap snd kvs))
       vals

allPairsAreInAssocList
    :: (Ord k, Show k, Show v, Eq k, Eq v, Hashable k)
    => HM.HashMap k v
    -> Core.MapModifier k v
    -> Bool
allPairsAreInAssocList hm mapMod =
    let kvs = HM.toList hm
        keysVals = Core.toList kvs mapMod
    in all (\(k, _) -> isJust . Core.lookup (flip HM.lookup hm) k $ mapMod) keysVals

mapModifierToHashMap :: (Eq k, Eq v, Hashable k) => [(k, v)] -> Bool
mapModifierToHashMap inserts =
    let newMod = foldl' (flip (uncurry Core.insert)) mempty inserts
        insMap = Core.insertionsMap newMod
    in insMap == (HM.fromList inserts)

mapModifierToList
    :: (Ord k, Show k, Show v, Eq k, Eq v, Hashable k)
    => Core.MapModifier k v
    -> Bool
mapModifierToList mapMod =
    Core.insertions mapMod == Core.toList [] mapMod

mapModifierToDeletionList
    :: (Ord k, Show k, Show v, Eq k, Eq v, Hashable k)
    => Core.MapModifier k v
    -> Bool
mapModifierToDeletionList mapMod =
    let deletions = Core.deletions mapMod
        keys = Core.keys [] . Core.filter isNothing $ mapMod
    in all (not . flip elem keys) deletions && all (not . flip elem deletions) keys

allPairsAreInMaybeList
    :: (Ord k, Show k, Show v1, Show v2, Eq k, Eq v1, Eq v2, Hashable k, v1 ~ v2)
    => (v1 -> Maybe v2)
    -> HM.HashMap k v2
    -> Core.MapModifier k v1
    -> Bool
allPairsAreInMaybeList f hm mapMod =
    let kv2s = HM.toList hm
        keysVal2s = Core.mapMaybe kv2s f mapMod
    in all (\(k, v) -> Core.lookup (flip HM.lookup hm) k mapMod == f v) keysVal2s

hashMapIsModified
    :: (Eq k, Eq v, Hashable k)
    => HM.HashMap k v
    -> Core.MapModifier k v
    -> Bool
hashMapIsModified hm mapMod =
    let modifiedHMap = Core.modifyHashMap mapMod hm
        inserts = Core.insertions mapMod
        deletes = Core.deletions mapMod
    in all (\(k, v) -> HM.lookup k modifiedHMap == Just v) inserts &&
       all (not . flip HM.member modifiedHMap) deletes
