{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Chronological sequences.

module Pos.Util.Chrono
       ( Chrono
       , NewestFirst(..)
       , OldestFirst(..)
       , toNewestFirst
       , toOldestFirst
       , NE
       ) where

import           Universum          hiding (mapMaybe)

import           Control.Lens       (makeWrapped, _Wrapped)
import qualified Control.Lens       as Lens (Each (..))
import           Data.Binary        (Binary)
import qualified Data.List.NonEmpty as NE
import           Data.Witherable    (Witherable (..))
import qualified GHC.Exts           as IL
import           Test.QuickCheck    (Arbitrary)

import           Pos.Binary.Class   (Bi)

newtype NewestFirst f a = NewestFirst {getNewestFirst :: f a}
  deriving (Eq, Ord, Show,
            Functor, Foldable, Traversable,
            Container, NontrivialContainer,
            Binary, Bi,
            Arbitrary)
newtype OldestFirst f a = OldestFirst {getOldestFirst :: f a}
  deriving (Eq, Ord, Show,
            Functor, Foldable, Traversable,
            Container, NontrivialContainer,
            Binary, Bi,
            Arbitrary)

makeWrapped ''NewestFirst
makeWrapped ''OldestFirst

instance Witherable t => Witherable (NewestFirst t) where
    mapMaybe f = over _Wrapped (mapMaybe f)

instance Witherable t => Witherable (OldestFirst t) where
    mapMaybe f = over _Wrapped (mapMaybe f)

instance Lens.Each (f a) (f b) a b =>
         Lens.Each (NewestFirst f a) (NewestFirst f b) a b where
    each = _Wrapped . Lens.each
instance Lens.Each (f a) (f b) a b =>
         Lens.Each (OldestFirst f a) (OldestFirst f b) a b where
    each = _Wrapped . Lens.each

instance One (f a) => One (NewestFirst f a) where
    type OneItem (NewestFirst f a) = OneItem (f a)
    one = NewestFirst . one
instance One (f a) => One (OldestFirst f a) where
    type OneItem (OldestFirst f a) = OneItem (f a)
    one = OldestFirst . one

instance IL.IsList (f a) => IL.IsList (NewestFirst f a) where
    type Item (NewestFirst f a) = IL.Item (f a)
    toList = IL.toList . getNewestFirst
    fromList = NewestFirst . IL.fromList

instance IL.IsList (f a) => IL.IsList (OldestFirst f a) where
    type Item (OldestFirst f a) = IL.Item (f a)
    toList = IL.toList . getOldestFirst
    fromList = OldestFirst . IL.fromList

class Chrono f where
    toNewestFirst :: OldestFirst f a -> NewestFirst f a
    toOldestFirst :: NewestFirst f a -> OldestFirst f a

instance Chrono [] where
    toNewestFirst = NewestFirst . reverse . getOldestFirst
    toOldestFirst = OldestFirst . reverse . getNewestFirst

instance Chrono NonEmpty where
    toNewestFirst = NewestFirst . NE.reverse . getOldestFirst
    toOldestFirst = OldestFirst . NE.reverse . getNewestFirst

type NE = NonEmpty
