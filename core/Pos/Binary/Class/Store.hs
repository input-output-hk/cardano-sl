{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Serialization-related types

module Pos.Binary.Class.Store
       (
         putConst
       , putField
       , appendConst
       , appendField
       , convertToSizeNPut
       -- * Store re-exports
       , Size(..)
       , Peek
       , Poke

       -- * Poke with size of the poke.
       , PokeWithSize (..)
       , pokeWithSize

       -- * The 'StaticSize' wrapper
       , StaticSize(..)

       -- * Primitives for limiting serialization
       , isEmptyPeek
       , lookAhead
       , mkPoke
       , execPoke

       , constSize
       , convertSize
       , combineSize
       , sizeOf
       , sizeAddField
       ) where

import           Universum

import           Data.Functor.Contravariant (contramap)
import           Data.Store                 (Size (..))
import           Data.Store.Core            (Peek (..), PeekResult (..), Poke (..))
import qualified Data.Store.Core            as Store
import           Data.Store.Internal        (PeekException (..), StaticSize (..))
import qualified Data.Store.Internal        as Store

import           Pos.Binary.Class.Core      (Bi (..), getSize)

----------------------------------------------------------------------------
-- Helpers to implement @sizeNPut@
----------------------------------------------------------------------------

-- [CSL-1122] shouldn't this mappend results as well? like Monoid (IO a) does
instance Monoid a => Monoid (Poke a) where
    mempty = pure mempty
    m1 `mappend` m2 = Poke $ \ps off -> do
        (off1, _) <- runPoke m1 ps off
        (off2, res) <- runPoke m2 ps off1
        pure (off2, res)

instance Monoid (Size a) where
    mempty = ConstSize 0
    m1 `mappend` m2 = case (m1, m2) of
        (VarSize f, VarSize g)     -> VarSize (\x -> f x + g x)
        (VarSize f, ConstSize m)   -> VarSize (\x -> f x + m)
        (ConstSize n, VarSize g)   -> VarSize (\x -> n + g x)
        (ConstSize n, ConstSize m) -> ConstSize (n + m)

-- | Helper for @sizeNPut@. Put const size object.
putConst :: Bi x => x -> (Size a, a -> Poke ())
putConst x = (ConstSize (getSize x), \_ -> put x)

-- | Helper for @sizeNPut@. Put field of datatype.
-- You can concat several @putField@ and @putConst@ together, for example:
-- data GenericBlockHeader b = UnsafeGenericBlockHeader
--     { _gbhPrevBlock :: !(BHeaderHash b)
--       _gbhBodyProof :: !(BodyProof b)
--       _gbhConsensus :: !(ConsensusData b)
--       _gbhExtra     :: !(ExtraHeaderData b)
--     } deriving (Generic)
-- sizeNPut = putConst protocolMagic <>
--            putField _gbhPrevBlock <>
--            putField _gbhBodyProof <>
--            putField _gbhConsensus <>
--            putField _gbhExtra
putField :: Bi x => (a -> x) -> (Size a, a -> Poke ())
putField f = (VarSize $ \a -> getSize (f a), put . f)
-- [CSL-1122] This should use 'convertSize'

-- | Yet another helper for const size object.
appendConst :: Bi x => (Size a, a -> Poke ()) -> x -> (Size a, a -> Poke ())
appendConst a x = a <> putConst x

-- | Yet another helper for field of datatype.
appendField :: Bi x => (Size a, a -> Poke ()) -> (a -> x) -> (Size a, a -> Poke ())
appendField a f = a <> putField f

----------------------------------------------------------------------------
-- Helpers to implement @size@
----------------------------------------------------------------------------

-- [CSL-1122] this is already defined in Pos.Binary.Class.Instances
constSize :: forall a . Bi a => Int
constSize =  case size :: Size a of
    VarSize   _ -> error "constSize: VarSize"
    ConstSize a -> a

-- | Helper for implementation of @Bi.size@.
-- You can get @Size a@ from @Size b@ if there is
-- way to convert a to b.
convertSize :: (a -> b) -> Size b -> Size a
convertSize = contramap
{-# INLINE convertSize #-}

-- | Helper for implementation of @Bi.size@.
-- Useful when data contains only one field, then size of data is @sizeOf field@
sizeOf :: Bi a => (x -> a) -> Size x
sizeOf conv = convertSize conv size
{-# INLINE sizeOf #-}

-- Instances for tuples with up to 5 elements are provided. There's a TH
-- generator in the neongreen/THBUG branch, but it doesn't work because,
-- well, there's a bug in TH.
--
-- [CSL-1122] TODO: benchmark 'sizeAddField' and 'combineSize' and either
-- document the difference or remove 'combineSize' if there's no difference
class CombineSize a b | a -> b where
    -- | If you have a record with fields @A {a, b, c}@ and you want to write
    -- a 'Bi' instance for it, you can write @size = combineSize (a, b, c)@.
    --
    -- If you have more than five fields, please write the following (which
    -- is probably a bit slower):
    --
    -- @ConstSize 0 `sizeAddField` a `sizeAddField` b `sizeAddField` ...@.
    combineSize :: a -> Size b

-- this could be written as “CombineSize (xt -> p1, xt -> p2) xt”, but the
-- way we do it here leads to better type inference because this instance
-- guarantees that it's the *only* possible instance for a tuple of length 2
instance (Bi a, xa ~ (x -> a),
          Bi b, xb ~ (x -> b))
         => CombineSize (xa, xb) x where
    combineSize (a, b) = Store.combineSizeWith a b size size
    {-# INLINE combineSize #-}

instance (Bi p1, x1 ~ (xt -> p1),
          Bi p2, x2 ~ (xt -> p2),
          Bi p3, x3 ~ (xt -> p3))
        => CombineSize (x1, x2, x3) xt where
   combineSize (f1, f2, f3) =
       case (size :: Size p1,
             size :: Size p2,
             size :: Size p3) of
           (ConstSize s1,
            ConstSize s2,
            ConstSize s3) -> ConstSize (s1 + s2 + s3)
           _ -> VarSize $ \xv -> getSize (f1 xv) +
                                 getSize (f2 xv) +
                                 getSize (f3 xv)
   {-# INLINE combineSize #-}

instance (Bi p1, x1 ~ (xt -> p1),
          Bi p2, x2 ~ (xt -> p2),
          Bi p3, x3 ~ (xt -> p3),
          Bi p4, x4 ~ (xt -> p4))
        => CombineSize (x1, x2, x3, x4) xt where
   combineSize (f1, f2, f3, f4) =
       case (size :: Size p1,
             size :: Size p2,
             size :: Size p3,
             size :: Size p4) of
           (ConstSize s1,
            ConstSize s2,
            ConstSize s3,
            ConstSize s4) -> ConstSize (s1 + s2 + s3 + s4)
           _ -> VarSize $ \xv -> getSize (f1 xv) +
                                 getSize (f2 xv) +
                                 getSize (f3 xv) +
                                 getSize (f4 xv)
   {-# INLINE combineSize #-}

instance (Bi p1, x1 ~ (xt -> p1),
          Bi p2, x2 ~ (xt -> p2),
          Bi p3, x3 ~ (xt -> p3),
          Bi p4, x4 ~ (xt -> p4),
          Bi p5, x5 ~ (xt -> p5))
        => CombineSize (x1, x2, x3, x4, x5) xt where
   combineSize (f1, f2, f3, f4, f5) =
       case (size :: Size p1,
             size :: Size p2,
             size :: Size p3,
             size :: Size p4,
             size :: Size p5) of
           (ConstSize s1,
            ConstSize s2,
            ConstSize s3,
            ConstSize s4,
            ConstSize s5) -> ConstSize (s1 + s2 + s3 + s4 + s5)
           _ -> VarSize $ \xv -> getSize (f1 xv) +
                                 getSize (f2 xv) +
                                 getSize (f3 xv) +
                                 getSize (f4 xv) +
                                 getSize (f5 xv)
   {-# INLINE combineSize #-}

sizeAddField :: forall a x. Bi a => Size x -> (x -> a) -> Size x
sizeAddField sizeX toA =
    case (sizeX, size @a) of
        (VarSize f, VarSize g)     -> VarSize (\x -> f x + g (toA x))
        (VarSize f, ConstSize m)   -> VarSize (\x -> f x + m)
        (ConstSize n, VarSize g)   -> VarSize (\x -> n + g (toA x))
        (ConstSize n, ConstSize m) -> ConstSize (n + m)
{-# INLINE sizeAddField #-}

infixl 9 `sizeAddField`

----------------------------------------------------------------------------
-- Poke with Size
----------------------------------------------------------------------------

-- | A wrapper around Poke wich stores also length of Poke,
-- useful for functions like @putWithLength@.
data PokeWithSize a
    = PokeWithSize {
      pwsToSize :: !Int
    , pwsToPoke :: !(Poke a)
    } deriving (Functor)

-- | Create @PokeWithSize@ from Bi a.
pokeWithSize :: Bi a => a -> PokeWithSize ()
pokeWithSize x = PokeWithSize (getSize x) (put x)

-- | Conversion between two equivalent formats.
-- Might be useful for @sizeNPut@ of datatype with several constructors.
convertToSizeNPut :: (a -> PokeWithSize ()) -> (Size a, a -> Poke ())
convertToSizeNPut f = (VarSize $ pwsToSize . f, pwsToPoke . f)

instance Monoid a => Monoid (PokeWithSize a) where
    mempty = PokeWithSize 0 (pure mempty)
    m1 `mappend` m2 =
        PokeWithSize  (pwsToSize m1 + pwsToSize m2) (pwsToPoke m1 <> pwsToPoke m2)

instance Applicative PokeWithSize where
    pure x = PokeWithSize 0 (pure x)
    {-# INLINE pure #-}
    PokeWithSize fsz f <*> PokeWithSize vsz v = PokeWithSize (fsz + vsz) (f <*> v)
    {-# INLINE (<*>) #-}
    PokeWithSize fsz f *> PokeWithSize vsz v = PokeWithSize  (fsz + vsz) (f *> v)
    {-# INLINE (*>) #-}

----------------------------------------------------------------------------
-- Useful functions for Poke and Peek.
----------------------------------------------------------------------------

-- | Test Peek on empty.
isEmptyPeek :: Peek Bool
isEmptyPeek = Peek $ \end ptr ->
    pure (PeekResult ptr (ptr >= Store.peekStateEndPtr end))

-- | Try to read @a@ but don't move pointer on the buffer, fail if can't.
lookAhead :: Peek a -> Peek a
lookAhead m = Peek $ \end ptr -> Store.runPeek m end ptr `catch` onEx
  where
    onEx (PeekException ptr exMsg) =
        throwM $ PeekException ptr (exMsg <> "\nlookAhead failed")

execPoke :: Poke a -> Store.PokeState -> Store.Offset -> IO Store.Offset
execPoke p ptr offset = fst <$> Store.runPoke p ptr offset

mkPoke
    :: (Store.PokeState -> Store.Offset -> IO Store.Offset)
    -> Poke ()
mkPoke f = Store.Poke (\ptr offset -> (,()) <$> f ptr offset)
