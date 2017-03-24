{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}

-- | A type for heterogeneous vectors.
module Pos.Util.HVect
       ( HVect
       , Contains
       , empty
       , cons
       , extract
       ) where

import           Universum        hiding (empty)

import           Data.Vector      (Vector)
import qualified Data.Vector      as V
import           GHC.TypeLits     (type (+))
import           Unsafe.Coerce    (unsafeCoerce)

-- | A heterogeneous vector. @HVect [Int, Bool, Char]@ is a vector with
-- three elements; the first one is of type 'Int', the second is of type
-- 'Bool', and the third is of type 'Char'.
--
-- The underlying type is a 'Vector', which means that the access to
-- individual elements is fast.
newtype HVect (xs :: [*]) = HVect (Vector Any)  -- 'Any' = untyped value

-- | An empty 'HVect'.
empty :: HVect '[]
empty = HVect mempty

-- | Add an element to a 'HVect'.
cons :: x -> HVect xs -> HVect (x ': xs)
cons x (HVect xs) = HVect (V.cons (unsafeCoerce x) xs)

-- | We can use 'Index' to find the position of a type in a list of
-- types. The result is a 'Nat', i.e. a type-level number.
type family Index (x :: *) (xs :: [*]) :: Nat where
    Index x (x ': _)  = 0
    Index x (_ ': xs) = 1 + Index x xs

type Contains x xs = KnownNat (Index x xs)

-- | Find the first occurence of some type in a 'HVect' and extract the value
-- of that type.
extract :: forall x xs. Contains x xs => HVect xs -> x
extract (HVect v) =
    let i = natVal (Proxy @(Index x xs))
    in  unsafeCoerce (v V.! fromIntegral i)
