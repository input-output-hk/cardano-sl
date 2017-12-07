module Data.NonEmptySet (

      NonEmptySet
    , singleton
    , insert
    , delete
    , deleteMany
    , toList

    ) where

import           Data.Foldable (foldrM)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Set (Set)
import qualified Data.Set as S

-- | A set (no duplicates) with at least one element.
data NonEmptySet t = NonEmptySet !t !(Set t)

instance Show t => Show (NonEmptySet t) where
    show = show . toList

-- | Construct a 'NonEmptySet' by giving one element.
singleton :: t -> NonEmptySet t
singleton t = NonEmptySet t S.empty

-- | Extend a 'NonEmptySet' by adding one element.
insert :: Ord t => t -> NonEmptySet t -> NonEmptySet t
insert t nes@(NonEmptySet t' set) = case t `compare` t' of
    LT -> NonEmptySet t (S.insert t' set)
    EQ -> nes
    GT -> NonEmptySet t' (S.insert t set)

-- | Remove an element from a 'NonEmptySet'. If it's the only element of the
--   set, 'Nothing' is given.
delete :: Ord t => t -> NonEmptySet t -> Maybe (NonEmptySet t)
delete t (NonEmptySet t' set) = case (t == t', S.minView set) of
    (False, _)               -> Just $ NonEmptySet t' (S.delete t set)
    (True, Nothing)          -> Nothing
    (True, Just (t'', set')) -> Just $ NonEmptySet t'' set'

deleteMany :: Ord t => [t] -> NonEmptySet t -> Maybe (NonEmptySet t)
deleteMany ts neset = foldrM delete neset ts

-- | Forget the fact that a 'NonEmptySet' and construct a 'NonEmpty' list, in
--   which duplicates *are* allowed.
toList :: NonEmptySet t -> NonEmpty t
toList (NonEmptySet t set) = t :| S.toList set
