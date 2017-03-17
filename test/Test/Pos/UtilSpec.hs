{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pos.Util specification

module Test.Pos.UtilSpec
       ( spec
       ) where

import           Control.Lens          (Each)
import qualified Data.HashMap.Strict   as HM (difference, filter, intersection,
                                              intersectionWith, keys, mapWithKey, member,
                                              (!))
import qualified Data.List.NonEmpty    as NE
import qualified GHC.Exts              as IL (IsList (..))
import           Pos.Types.Arbitrary   (SmallHashMap (..))
import           Pos.Util              (Chrono (..), NewestFirst (..), OldestFirst (..),
                                        diffDoubleMap)

import           Test.Hspec            (Expectation, Spec, describe, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.Pos.Util         ((.=.))
import           Test.QuickCheck       (Arbitrary, Property)
import           Universum

spec :: Spec
spec = describe "Util" $ do
    describe "diffDoubleMap" $ do
        prop description_ddmEmptyHashMap ddmEmptyHashMap
        prop description_doubleDiffDoesNothing doubleDiffDoesNothing
        prop description_verifyMapsAreSubtracted verifyMapsAreSubtracted
        prop description_verifyKeyIsPresent verifyKeyIsPresent
        prop description_verifyDiffMapIsSmaller verifyDiffMapIsSmaller
    describe "One" $ do
        prop description_One (toSingleton @[] @String NewestFirst)
        prop description_One (toSingleton @NE.NonEmpty @String OldestFirst)
        prop description_One (toSingleton @[] @String OldestFirst)
        prop description_One (toSingleton @NE.NonEmpty @String OldestFirst)
    describe "IsList" $ do
        describe "toList . fromList = id" $ do
            prop (description_toFromListNew "[]") (toFromList @[] @NewestFirst @String)
            prop (description_toFromListNew "NonEmpty")
                (toFromList @NE.NonEmpty @NewestFirst @String)
            prop (description_toFromListOld "[]") (toFromList @[] @OldestFirst @String)
            prop (description_toFromListOld "NonEmpty")
                (toFromList @NE.NonEmpty @OldestFirst @String)
    describe "Chrono" $ do
        prop (description_fromOldestToNewest "[]") (fromOldestToNewest @[] @String)
        prop (description_fromOldestToNewest "NonEmpty")
            (fromOldestToNewest @NE.NonEmpty @String)
        prop (description_fromNewestToOldest "[]") (fromNewestToOldest @[] @String)
        prop (description_fromNewestToOldest "NonEmpty")
            (fromNewestToOldest @NE.NonEmpty @String)
          where
    description_ddmEmptyHashMap =
        "Removing an empty double hashmap from another does nothing, and removing a\
        \ double hashmap from itself results in an empty hashmap"
    description_doubleDiffDoesNothing =
        "Removing a double hashmap from another twice in a row is the same as doing it\
        \ only once"
    description_verifyMapsAreSubtracted =
        "Applying 'diffDoubleMap' to a double hashmap hm1 with a map hm2 will subtract\
        \ the submaps (whose key exists in hm2) from those sharing the same key in hm1, \
        \ and do nothing to the submaps in hm1 otherwise."
    description_verifyKeyIsPresent =
        "If a key is present in a subtraction between two maps, then it will be present\
        \ in the minuend hashmap"
    description_verifyDiffMapIsSmaller =
        "When subtracting two double hashmaps with common keys, and when the inner maps\
        \ corresponding to these keys have a non-empty intersection, the difference\
        \ map's inner maps corresponding to those keys will be smaller in size than the\
        \ inner maps in the minuend hashmap"
    description_One =
        "Turning a single element into a chronological is the same as turning into the\
        \ structure within and applying the outer data constructor."
    description_toFromListNew functor =
        "Converting 'NewestFirst " ++ functor ++ " a' to '[Item a]' and back changes\
        \ nothing"
    description_toFromListOld functor =
        "Converting 'OldestFirst " ++ functor ++ " a' to '[Item a]' and back changes\
        \ nothing"
    description_fromOldestToNewest functor =
        "Converting 'NewestFirst " ++ functor ++ " a' to 'OldestFirst " ++ functor ++
        " a' and back again changes nothing"
    description_fromNewestToOldest functor =
        "Converting 'OldestFirst " ++ functor ++ " a' to 'NewestFirst " ++ functor ++
        " a' and back again changes nothing"


ddmEmptyHashMap
    :: SmallHashMap
    -> Bool
ddmEmptyHashMap (SmallHashMap hm1) =
    let ddm = diffDoubleMap
        hasIdentity =
            let id1 = mempty `ddm` hm1
                id2 = hm1 `ddm` mempty
            in id1 == mempty && hm1 == id2
        hasInverse =
            let inv1 = hm1 `ddm` hm1
            in inv1 == mempty
    in hasIdentity && hasInverse

doubleDiffDoesNothing
    :: SmallHashMap
    -> SmallHashMap
    -> Bool
doubleDiffDoesNothing (SmallHashMap hm1) (SmallHashMap hm2) =
    let diff1 = hm1 `diffDoubleMap` hm2
        diff2 = diff1 `diffDoubleMap` hm2
    in diff1 == diff2

verifyMapsAreSubtracted
    :: SmallHashMap
    -> SmallHashMap
    -> Bool
verifyMapsAreSubtracted (SmallHashMap hm1) (SmallHashMap hm2) =
    let diffMap = hm1 `diffDoubleMap` hm2
        checkIsDiff pk innerMap
            | HM.member pk hm1 && HM.member pk hm2 =
                (hm1 HM.! pk) `HM.difference` (hm2 HM.! pk) == innerMap
            | otherwise = hm1 HM.! pk == innerMap
    in and $ HM.mapWithKey checkIsDiff diffMap

verifyKeyIsPresent
    :: SmallHashMap
    -> SmallHashMap
    -> Bool
verifyKeyIsPresent (SmallHashMap hm1) (SmallHashMap hm2) =
    let diffMap = hm1 `diffDoubleMap` hm2
        diffKeys = HM.keys diffMap
        checkKeyIsPresent pk =
            let isPresentInFirstMap = HM.member pk hm1 && (not $ HM.member pk hm2)
                isPresentInDiffMap = HM.member pk diffMap
            in (not isPresentInFirstMap) || isPresentInDiffMap -- ((not p) || q) <=> (p => q)
    in all checkKeyIsPresent diffKeys

-- | This test does the following:
-- Given two double hashmaps and their difference, hm1, hm2, and diffMap, the
-- first two are intersected with a function that returns a hashmap from the
-- keys common to both maps to a Bool - True when the intersection of the inner
-- maps in hm1 and hm2 is not empty. If there is any key for which this occurs,
-- it is checked that the sum of the sizes of all inner hashmaps in hm1 is
-- strictly greater than those in diffMap.
--
-- In logical terms:
-- ∃  k : k ∈ hm1 ⋀ k ∈ hm2 ⋀ (hm1 ! k ⋂ hm2 ! k ≠ ∅) ⇒
-- Σ (| v1 |, v1 ∈ elems(hm1)) > Σ (| v |, v ∈ elems(diffMap))
verifyDiffMapIsSmaller
    :: SmallHashMap
    -> SmallHashMap
    -> Bool
verifyDiffMapIsSmaller (SmallHashMap hm1) (SmallHashMap hm2) =
    let diffMap = hm1 `diffDoubleMap` hm2
        innerFun inner1 inner2 = not $ null $ HM.intersection inner1 inner2
        commonKey = HM.filter identity $ HM.intersectionWith innerFun hm1 hm2
        sumValSizes = sum . fmap length
    -- (p || q) <=> ((not p) => q)
    in (null commonKey) || (sumValSizes hm1 > sumValSizes diffMap)

toSingleton
    :: forall f a t. (Arbitrary a,
                  Show (t f a),
                  Eq (t f a),
                  One (f a),
                  One (t f a),
                  OneItem (f a) ~ OneItem (t f a))
    => (f a -> t f a) -> OneItem (t f a) -> Expectation
toSingleton constructor x = (one x) `shouldBe` (constructor . one $ x)

toSingletonN :: (Arbitrary a, Show a, Eq a) => a -> Expectation
toSingletonN x = one x `shouldBe` (NewestFirst @[] . one $ x)

toSingletonO
    :: (Arbitrary a, Show a, Eq a)
    => a
    -> Expectation
toSingletonO x = one x `shouldBe` (OldestFirst @[] . one $ x)

toFromList
    :: forall f t a. (Arbitrary (t f a),
                  Show (t f a),
                  Eq (t f a),
                  IL.IsList (f a),
                  IL.IsList (t f a))
    => t f a
    -> Property
toFromList = IL.fromList . IL.toList .=. identity

fromOldestToNewest
    :: (Arbitrary (f a), Show (f a), Eq (f a), Chrono f)
    => OldestFirst f a
    -> Property
fromOldestToNewest = toOldestFirst . toNewestFirst .=. identity

fromNewestToOldest
    :: (Arbitrary (f a), Show (f a), Eq (f a), Chrono f)
    => NewestFirst f a
    -> Property
fromNewestToOldest = toNewestFirst . toOldestFirst .=. identity
