{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.Kernel.Util.Strict (
    module Strict
 ) where

import           Control.Lens.Iso (Strict, iso, lazy, strict)
import           Universum hiding (Maybe)
import qualified Universum as U

import           Test.QuickCheck (Arbitrary, Gen, arbitrary)

import           Data.SafeCopy (SafeCopy (..), contain, safeGet, safePut)
import qualified Formatting.Buildable

import           Data.Strict.Maybe as Strict

{--
    We reuse Lens' built-in 'Strict' typeclass to create a witness (i.e an
    'Iso') that we can convert back and forth between the lazy and strict
    variants of these types
--}

instance Strict (U.Maybe a) (Strict.Maybe a) where
    strict = iso toStrictMaybe toLazyMaybe

toStrictMaybe :: U.Maybe a -> Strict.Maybe a
toStrictMaybe U.Nothing  = Strict.Nothing
toStrictMaybe (U.Just a) = Strict.Just a

toLazyMaybe :: Strict.Maybe a -> U.Maybe a
toLazyMaybe Strict.Nothing  = U.Nothing
toLazyMaybe (Strict.Just a) = U.Just a

instance SafeCopy a => SafeCopy (Strict.Maybe a) where
    getCopy = contain $ do
        x <- safeGet
        pure $ case x of
             U.Nothing -> Strict.Nothing
             U.Just a  -> Strict.Just a
    putCopy x = contain $ do
        case x of
             Strict.Nothing -> safePut (U.Nothing :: U.Maybe a)
             Strict.Just a  -> safePut (U.Just a)

instance Buildable a => Buildable (Strict.Maybe a) where
    build = Formatting.Buildable.build @(U.Maybe a) . view lazy

instance Arbitrary a => Arbitrary (Strict.Maybe a) where
    arbitrary = view strict <$> (arbitrary :: Gen (U.Maybe a))
