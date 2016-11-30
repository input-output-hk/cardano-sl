{-# LANGUAGE RankNTypes #-}
-- | Pos.Util specification

module Test.Pos.UtilSpec
       ( spec
       ) where

import qualified Data.HashMap.Strict as HM ((!), difference, mapWithKey, member)
import           Pos.Types.Arbitrary       (SmallHashMap (..))
import           Pos.Util                  (diffDoubleMap)

import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (prop)
import           Universum

spec :: Spec
spec = describe "Util" $ do
    describe "diffDoubleMap" $ do
        prop description_ddmEmptyHashMap ddmEmptyHashMap
        prop description_doubleDiffDoesNothing doubleDiffDoesNothing
        prop description_verifyMapsAreSubtracted verifyMapsAreSubtracted
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

ddmEmptyHashMap
    :: SmallHashMap
    -> Bool
ddmEmptyHashMap (SmallHashMap hm1) =
    let ddm = diffDoubleMap
        hasIdentity =
            let id1 = mempty `ddm` hm1
                id2 = hm1 `ddm` mempty
            in (id1 == mempty) && (hm1 == id2)
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
