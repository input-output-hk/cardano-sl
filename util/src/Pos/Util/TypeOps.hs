-- | This module yanked from serokell's prelude "universum", which used
-- the type-operators library, which has since changed the kind of the
-- <+> thing.

{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Pos.Util.TypeOps
       ( type Each
       , type With
       , type ($)
       ) where

import Data.Kind (Constraint)

-- | Infix application.
--
-- @
-- f :: Either String $ Maybe Int
-- =
-- f :: Either String (Maybe Int)
-- @
--
-- copied from type-operators 0.1.0.4
type f $ a = f a
infixr 2 $

-- | Map several constraints over a single variable.
--
-- @
-- a :: [Show, Read] \<+> a => a -> a
-- =
-- a :: (Show a, Read a) => a -> a
-- @
--
-- copied from type-operators 0.1.0.4
type family (<+>) (c :: [k -> Constraint]) (a :: k) where
    (<+>) '[] a = (() :: Constraint)
    (<+>) (ch ': ct) a = (ch a, (<+>) ct a)
infixl 9 <+>

-- | Map several constraints over several variables.
--
-- @
-- f :: Each [Show, Read] [a, b] => a -> b -> String
-- =
-- f :: (Show a, Show b, Read a, Read b) => a -> b -> String
-- @
--
-- To specify list with single constraint / variable, don't forget to prefix
-- it with @\'@:
--
-- @
-- f :: Each '[Show] [a, b] => a -> b -> String
-- @
--
-- copied from universum
type family Each (c :: [k -> Constraint]) (as :: [k]) where
    Each c '[] = (() :: Constraint)
    Each c (h ': t) = (c <+> h, Each c t)

-- | Map several constraints over a single variable.
-- Note, that @With a b ≡ Each a '[b]@
--
-- @
-- a :: With [Show, Read] a => a -> a
-- =
-- a :: (Show a, Read a) => a -> a
-- @
--
-- copied from universum
type With a b = a <+> b
