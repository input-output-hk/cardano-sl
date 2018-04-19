{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TypeFamilies  #-}

module Pos.Communication.Limits.Types
       ( Limit (..)
       , (<+>)
       , mlBool
       , mlMaybe
       , mlEither
       , mlTuple
       , mlTriple
       , vectorOf
       , vectorOfNE
       ) where

import           Universum

-- | A limit on the length of something (in bytes).
--   TODO should check for overflow in the Num instance.
--   Although, if the limit is anywhere near maxBound :: Word32 then something
--   is almost certainly amiss.
newtype Limit t = Limit { getLimit :: Word32 }
    deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

instance Functor Limit where
    fmap _ (Limit x) = Limit x

-- TODO: use <*> instead of <+>
infixl 4 <+>
(<+>) :: Limit (a -> b) -> Limit a -> Limit b
Limit x <+> Limit y = Limit $ x + y

mlBool :: Limit Bool
mlBool = 1

mlMaybe :: Limit a -> Limit (Maybe a)
mlMaybe lim = Just <$> lim + 1

mlEither :: Limit a -> Limit b -> Limit (Either a b)
mlEither limA limB = 1 + max (Left <$> limA) (Right <$> limB)

mlTuple :: Limit a -> Limit b -> Limit (a, b)
mlTuple limA limB = (,) <$> limA <+> limB

mlTriple :: Limit a -> Limit b -> Limit c -> Limit (a, b, c)
mlTriple limA limB limC = (,,) <$> limA <+> limB <+> limC

-- | Given a limit for a list item, generate limit for a list with N elements
vectorOf :: Int -> Limit l -> Limit [l]
vectorOf k (Limit x) =
    Limit $ encodedListLength + x * (fromIntegral k)
  where
    -- should be enough for most reasonable cases
    -- FIXME this is silly.
    -- Better solution: never read in an arbitrary-length structure from the
    -- network. If you want a list, read in one item at a time.
    encodedListLength = 20

vectorOfNE :: Int -> Limit l -> Limit (NonEmpty l)
vectorOfNE k (Limit x) =
    Limit $ encodedListLength + x * (fromIntegral k)
  where
    encodedListLength = 20
