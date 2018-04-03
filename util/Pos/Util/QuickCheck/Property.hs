module Pos.Util.QuickCheck.Property
       ( -- * Various properties and predicates
         qcIsJust
       , qcIsNothing
       , qcIsLeft
       , qcIsRight
       , qcElem
       , qcNotElem
       , qcFail

       -- * Monadic properties
       , assertProperty
       , stopProperty
       , maybeStopProperty
       , splitIntoChunks
       , expectedOne

       -- * HSpec utils
       , expectationError

       -- * Generators
       , splitWord
       , sumEquals

       -- * Helpers
       , (.=.)
       , (>=.)
       , shouldThrowException

       -- * Semigroup/monoid laws
       , formsSemigroup
       , formsMonoid
       , formsCommutativeMonoid
       ) where

import           Universum

import qualified Data.Semigroup as Semigroup
import           Test.Hspec (Expectation, Selector, shouldThrow)
import           Test.QuickCheck (Arbitrary, Property, counterexample, property, (.&&.), (===))
import           Test.QuickCheck.Gen (Gen, choose)
import           Test.QuickCheck.Monadic (PropertyM, pick, stop)
import           Test.QuickCheck.Property (Result (..), failed)

----------------------------------------------------------------------------
-- Various properties and predicates
----------------------------------------------------------------------------

qcIsJust :: Maybe a -> Property
qcIsJust (Just _) = property True
qcIsJust Nothing  = qcFail "expected Just, got Nothing"

qcIsNothing :: Show a => Maybe a -> Property
qcIsNothing Nothing  = property True
qcIsNothing (Just x) = qcFail ("expected Nothing, got Just (" <> show x <> ")")

qcIsLeft :: Show b => Either a b -> Property
qcIsLeft (Left _)  = property True
qcIsLeft (Right x) = qcFail ("expected Left, got Right (" <> show x <> ")")

qcIsRight :: Show a => Either a b -> Property
qcIsRight (Right _) = property True
qcIsRight (Left x)  = qcFail ("expected Right, got Left (" <> show x <> ")")

-- TBD would it not be sufficient to just use 'Traversable'? Does the
-- 'Container' class from 'universum' add any value?
--
--   qcElem :: (Eq a, Show a, Show (t a), Traversable t) => a -> t a -> Property
--
qcElem
    :: (Eq a, Show a, Show t, Container t, Element t ~ a, ElementConstraint t a)
    => a -> t -> Property
qcElem x xs =
    counterexample ("expected " <> show x <> " to be in " <> show xs) $
    x `elem` xs

qcNotElem
    :: (Eq a, Show a, Show t, Container t, Element t ~ a, ElementConstraint t a)
    => a -> t -> Property
qcNotElem x xs =
    counterexample ("expected " <> show x <> " not to be in " <> show xs) $
    not (x `elem` xs)

-- | A property that is always false
qcFail :: Text -> Property
qcFail s = counterexample (toString s) False

----------------------------------------------------------------------------
-- Monadic testing
----------------------------------------------------------------------------

-- | Call stopProperty if boolean value is false.
assertProperty :: Monad m => Bool -> Text -> PropertyM m ()
assertProperty st text = unless st $ stopProperty text

-- Note, 'fail' does the same thing, but:
-- • it's quite trivial, almost no copy-paste;
-- • it's 'fail' from 'Monad', not 'MonadFail';
-- • I am not a fan of 'fail'.
-- | Stop 'PropertyM' execution with given reason. The property will fail.
stopProperty :: Monad m => Text -> PropertyM m a
stopProperty msg = stop failed {reason = toString msg}

-- | Use 'stopProperty' if the value is 'Nothing' or return something
-- it the value is 'Just'.
maybeStopProperty :: Monad m => Text -> Maybe a -> PropertyM m a
maybeStopProperty msg =
    \case
        Nothing -> stopProperty msg
        Just x -> pure x

-- | Split given list into chunks with size up to given value.
-- TODO: consider using `sumEquals maxSize (length items)`
splitIntoChunks :: Monad m => Word -> [a] -> PropertyM m [NonEmpty a]
splitIntoChunks 0 _ = error "splitIntoChunks: maxSize is 0"
splitIntoChunks maxSize items = do
    sizeMinus1 <- pick $ choose (0, maxSize - 1)
    let (chunk, rest) = splitAt (fromIntegral sizeMinus1 + 1) items
    case nonEmpty chunk of
        Nothing      -> return []
        Just chunkNE -> (chunkNE :) <$> splitIntoChunks maxSize rest

expectedOne :: Monad m => Text -> [a] -> PropertyM m a
expectedOne desc = \case
    [] ->  kickOut "expected exactly one element, but list is empty"
    [x] -> pure x
    _ ->   kickOut "expected exactly one element, but list contains more elements"
  where
    kickOut err = stopProperty $ err <> " (" <> desc <> ")"

----------------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------------

-- | Split given integer `total` into `parts` parts
-- TODO: improve naming!
splitWord :: Word64 -> Word64 -> Gen [Word64]
splitWord total parts | total < parts =
    error $ "splitWord: can't split " <> show total <> " into " <> show parts <> " parts."
                      | otherwise = map succ . take iParts <$> ((<> replicate iParts 0) <$> (sumEquals (total `div` parts + 1) $ total - parts))
  where
    iParts = fromIntegral parts

-- | Generate list of arbitrary positive integers which sum equals given sum.
-- All elements in the list will be smaller or equal then first parameter
sumEquals :: Word64 -> Word64 -> Gen [Word64]
sumEquals 0 _ = pure []
sumEquals _ 0 = pure []
sumEquals maxEl restSum = do
    el <- choose (1, min maxEl restSum)
    (el:) <$> sumEquals maxEl (restSum - el)

expectationError :: Text -> Expectation
expectationError = fail . toString

----------------------------------------------------------------------------
-- Monoid/Semigroup laws
----------------------------------------------------------------------------

isAssociative :: (Show m, Eq m, Semigroup m) => m -> m -> m -> Property
isAssociative m1 m2 m3 =
    let assoc1 = (m1 Semigroup.<> m2) Semigroup.<> m3
        assoc2 = m1 Semigroup.<> (m2 Semigroup.<> m3)
    in assoc1 === assoc2

formsSemigroup :: (Show m, Eq m, Semigroup m) => m -> m -> m -> Property
formsSemigroup = isAssociative

hasIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Property
hasIdentity m =
    let id1 = mempty Semigroup.<> m
        id2 = m Semigroup.<> mempty
    in (m == id1) .&&. (m == id2)

formsMonoid :: (Show m, Eq m, Semigroup m, Monoid m) => m -> m -> m -> Property
formsMonoid m1 m2 m3 =
    (formsSemigroup m1 m2 m3) .&&. (hasIdentity m1)

isCommutative :: (Show m, Eq m, Semigroup m, Monoid m) => m -> m -> Property
isCommutative m1 m2 =
    let comm1 = m1 <> m2
        comm2 = m2 <> m1
    in comm1 === comm2

formsCommutativeMonoid :: (Show m, Eq m, Semigroup m, Monoid m) => m -> m -> m -> Property
formsCommutativeMonoid m1 m2 m3 =
    (formsMonoid m1 m2 m3) .&&. (isCommutative m1 m2)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Extensional equality combinator. Useful to express function properties as functional
-- equations.
(.=.) :: (Eq b, Show b, Arbitrary a) => (a -> b) -> (a -> b) -> a -> Property
(.=.) f g a = f a === g a

infixr 5 .=.

-- | Monadic extensional equality combinator.
(>=.)
    :: (Show (m b), Arbitrary a, Monad m, Eq (m b))
    => (a -> m b)
    -> (a -> m b)
    -> a
    -> Property
(>=.) f g a = f a === g a

infixr 5 >=.

shouldThrowException
    :: (Show a, Eq a, Exception e)
    => (a -> b)
    -> Selector e
    -> a
    -> Expectation
shouldThrowException action exception arg =
    (return $! action arg) `shouldThrow` exception
