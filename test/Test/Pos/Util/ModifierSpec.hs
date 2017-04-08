{-# LANGUAGE GADTs #-}

-- | Specification for Pos.Util.Modifier (from 'core')

module Test.Pos.Util.ModifierSpec
       ( spec
       ) where

import qualified Data.HashMap.Strict   as HM

import qualified Pos.Util.Modifier     as Core
import           Universum

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.Pos.Util         (formsMonoid)
import           Test.QuickCheck   (Property)

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
                "inserting a value and then deleting it removes it from the map"
                (insertDeleteThenLookup @Text @Text)
        describe "keys" $ do
            prop
                "getting the keys from a map modifier does not eliminate any from the\
                \ produced list, unless their value was 'Nothing' or they are in the\
                \ argument list and are a key in the map modifier simultanously"
                (allKeysAreInMap @Text @Text)
        describe "values" $ do
            prop
                "getting the values from a map modifier does not eliminate any from the\
                \ produced list, unless their value is 'Nothing' or if it is the value\
                \ of a key in the input list that does not belong to the the map modifier"
                (allValsAreInMap @Text @Text)
        describe "toList" $ do
            prop
                "getting the contents of the map modifier as an association list will\
                \ keep all of the keys whose values are not 'Nothing', and will include\
                \ the pairs in the argument list whose key is not present in the modifier"
                (allPairsAreInAssocList @Text @Text)
        describe "insertionsMap" $ do
            prop
                "the created hashmap removes all the 'Nothing' keys from the\
                \ 'MapModifier' and keeps all the 'Just' keys"
                (mapModifierToHashMap @Text @Text)
        describe "insertions" $ do
            prop
                "the created list removes all the 'Nothing' keys from the used\
                \ 'MapModifier' and keeps all the 'Just' keys in the association list"
                (mapModifierToList @Text @Text)
        describe "deletions" $ do
            prop
                "the created list of keys removes all those with 'Just' values and keeps\
                \ those with 'Nothing' values"
                (mapModifierToDeletionList @Text @Text)
        describe "mapMaybe" $ do
            prop
                "the created list of keys removes all those with 'Nothing' values and\
                \ keeps the pairs from the input list whose keys are not in the modifier"
                (allPairsAreInMaybeList @Text @Text @Text Just)


mapModifierFormsMonoid
    :: (Show k, Show v, Eq k, Eq v, Hashable k)
    => Core.MapModifier k v
    -> Core.MapModifier k v
    -> Core.MapModifier k v
    -> Property
mapModifierFormsMonoid = formsMonoid

insertThenLookup
    :: (Show k, Show v, Eq k, Eq v, Hashable k)
    => k
    -> v
    -> Core.MapModifier k v
    -> Bool
insertThenLookup k v mapMod =
    let newMap = Core.insert k v mapMod
    in maybe False (== v) $ Core.lookup (const Nothing) k newMap

insertDeleteThenLookup
    :: (Show k, Show v, Eq k, Eq v, Hashable k)
    => k
    -> v
    -> Core.MapModifier k v
    -> Bool
insertDeleteThenLookup k v mapMod =
    let newMap@(Core.MapModifier mm) = Core.insert k v mapMod
        (Core.MapModifier dm) = Core.delete k newMap
    in (HM.lookup k mm == Just (Just v)) && (HM.lookup k dm == Just Nothing)

allKeysAreInMap
    :: (Ord k, Show k, Show v, Eq k, Eq v, Hashable k)
    => [k]
    -> Core.MapModifier k v
    -> Bool
allKeysAreInMap ks mapMod@(Core.MapModifier mm) =
    let keys = Core.keys ks mapMod
    in all (\k -> (isJust $ HM.lookup k mm) || (not $ HM.member k mm)) keys

allValsAreInMap
    :: (Ord k, Show k, Show v, Eq k, Eq v, Hashable k)
    => [(k,v)]
    -> Core.MapModifier k v
    -> Bool
allValsAreInMap kvs mapMod@(Core.MapModifier mm) =
    let vals = Core.values kvs mapMod
    in all (\v -> (Just v `elem` HM.elems mm) || (v `elem` (fmap snd kvs))) vals

allPairsAreInAssocList
    :: (Ord k, Show k, Show v, Eq k, Eq v, Hashable k)
    => [(k, v)]
    -> Core.MapModifier k v
    -> Bool
allPairsAreInAssocList kvs mapMod@(Core.MapModifier mm) =
    let keysVals = Core.toList kvs mapMod
    in all (\(k, _) -> (isJust . join . HM.lookup k) mm || not (HM.member k mm)) keysVals

mapModifierToHashMap
    :: (Ord k, Show k, Show v, Eq k, Eq v, Hashable k)
    => Core.MapModifier k v
    -> Bool
mapModifierToHashMap mapMod@(Core.MapModifier mm) =
    (fmap Just . Core.insertionsMap) mapMod == HM.filter isJust mm

mapModifierToList
    :: (Ord k, Show k, Show v, Eq k, Eq v, Hashable k)
    => Core.MapModifier k v
    -> Bool
mapModifierToList mapMod@(Core.MapModifier mm) =
    (fmap (second Just) . Core.insertions) mapMod == (HM.toList . HM.filter isJust) mm

mapModifierToDeletionList
    :: (Ord k, Show k, Show v, Eq k, Eq v, Hashable k)
    => Core.MapModifier k v
    -> Bool
mapModifierToDeletionList mapMod@(Core.MapModifier mm) =
    Core.deletions mapMod == (HM.keys . HM.filter isNothing) mm

allPairsAreInMaybeList
    :: (Ord k, Show k, Show v1, Show v2, Eq k, Eq v1, Eq v2, Hashable k, v1 ~ v2)
    => (v1 -> Maybe v2)
    -> [(k, v2)]
    -> Core.MapModifier k v1
    -> Bool
allPairsAreInMaybeList f kv2s mapMod@(Core.MapModifier mm) =
    let keysVal2s = Core.mapMaybe kv2s f mapMod
    in all (\(k, v) -> HM.lookup k mm == Just (f v) || not (HM.member k mm)) keysVal2s
