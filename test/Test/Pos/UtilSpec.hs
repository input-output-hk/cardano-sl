{-# LANGUAGE RankNTypes #-}
-- | Pos.Util specification

module Test.Pos.UtilSpec
       ( spec
       ) where

import qualified Data.HashMap.Strict   as HM (difference, filter, intersection,
                                              intersectionWith, keys, mapWithKey, member,
                                              (!))
import           Pos.Types.Arbitrary   (SmallHashMap (..))
import           Pos.Util              (diffDoubleMap)

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

spec :: Spec
spec = describe "Util" $ do
    describe "diffDoubleMap" $ do
        prop description_ddmEmptyHashMap ddmEmptyHashMap
        prop description_doubleDiffDoesNothing doubleDiffDoesNothing
        prop description_verifyMapsAreSubtracted verifyMapsAreSubtracted
        prop description_verifyKeyIsPresent verifyKeyIsPresent
        prop description_verifyDiffMapIsSmaller verifyDiffMapIsSmaller
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
