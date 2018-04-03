{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pos.Util specification

module Test.Pos.UtilSpec
       ( spec
       ) where

import           Universum

import qualified Data.List.NonEmpty as NE
import qualified GHC.Exts as IL (IsList (..))

import           Pos.Util.Chrono (Chrono (..), NewestFirst (..), OldestFirst (..))
import           Pos.Util.QuickCheck.Property ((.=.))

import           Test.Hspec (Expectation, Spec, describe, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary (..), Property, listOf1)

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = NE.fromList <$> listOf1 arbitrary

spec :: Spec
spec = describe "Util" $ do
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

toSingleton
    :: forall f a t. (Arbitrary a,
                  One (f a),
                  Each [Show, Eq, One] '[t f a],
                  OneItem (f a) ~ OneItem (t f a))
    => (f a -> t f a) -> OneItem (t f a) -> Expectation
toSingleton constructor x = (one x) `shouldBe` (constructor . one $ x)

toFromList
    :: forall f t a. (Each [Arbitrary, Show, Eq, IL.IsList] '[t f a], IL.IsList (f a))
    => t f a
    -> Property
toFromList = IL.fromList . IL.toList .=. identity

fromOldestToNewest
    :: (Each [Arbitrary, Show, Eq] '[f a], Chrono f)
    => OldestFirst f a
    -> Property
fromOldestToNewest = toOldestFirst . toNewestFirst .=. identity

fromNewestToOldest
    :: (Each [Arbitrary, Show, Eq] '[f a], Chrono f)
    => NewestFirst f a
    -> Property
fromNewestToOldest = toNewestFirst . toOldestFirst .=. identity
