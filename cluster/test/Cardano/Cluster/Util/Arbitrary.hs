{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Cluster.Util.Arbitrary
    (
    -- * Strings
      SeparatedBy(..)
    , UpperCased(..)
    , PrefixedEnv(..)

    -- * List
    , SmallList(..)
    ) where

import qualified Prelude
import           Universum hiding (keys)

import qualified Data.Char as C
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import           Test.QuickCheck (Arbitrary (..), Gen, elements, listOf, oneof,
                     scale, sized)

import           Cardano.Cluster.Util (split)


--
-- Types
--

newtype SeparatedBy (sep :: Symbol) =
    SeparatedBy { getSeparatedBy :: String }
    deriving (Show)

newtype UpperCased str =
    UpperCased { getUpperCased :: String }
    deriving (Show)

newtype PrefixedEnv (prefix :: Symbol) a =
    PrefixedEnv { getPrefixedEnv :: [(String, a)] }
    deriving (Show)

newtype SmallList a =
    SmallList  { getSmallList :: [a] }
    deriving (Show)


--
-- Instances
--

instance MkSeparatedBy "/"
instance MkSeparatedBy "-"
instance MkSeparatedBy "_"
instance MkSeparatedBy sep => Arbitrary (SeparatedBy sep) where
    arbitrary =
        let
            s = [sep (Proxy @sep)]
        in
            mkSeparatedBy <$> listOf (oneof
                [ elements someWords
                , (<> s) <$> elements someWords
                , (s <>) <$> elements someWords
                ])

    shrink (SeparatedBy s)
        | s == ""   = []
        | otherwise = mkSeparatedBy <$>
                Prelude.init (subsequences (split (sep (Proxy @sep)) s))


instance MkUpperCased String where to = identity; from = identity
instance MkUpperCased (SeparatedBy sep) where to = getSeparatedBy; from = SeparatedBy
instance (MkUpperCased s, Arbitrary s) => Arbitrary (UpperCased s) where
    arbitrary             = mkUpperCased <$> (arbitrary :: Gen s)
    shrink (UpperCased s) = mkUpperCased <$> shrink (from s :: s)


instance (KnownSymbol prefix, Arbitrary a) => Arbitrary (PrefixedEnv prefix a) where
    shrink (PrefixedEnv env) = PrefixedEnv <$> shrink env
    arbitrary = do
        values <- sized (`replicateM` arbitrary)
        keys   <- sized (`replicateM` (elements someWords))
        let prefix = symbolVal (Proxy @prefix)
        env <- forM (zip keys values) $ \(k, v) -> oneof
            [ pure (prefix <> k, v)
            , pure (k, v)
            ]
        return (PrefixedEnv env)


instance Arbitrary a => Arbitrary (SmallList a) where
    shrink (SmallList xs) = SmallList <$> shrink xs
    arbitrary = SmallList <$> scale (`mod` 10) arbitrary


--
-- Internal
--

class KnownSymbol sep => MkSeparatedBy sep where
    sep :: Proxy sep -> Char
    sep = Prelude.head . symbolVal
    mkSeparatedBy :: [String] -> SeparatedBy sep
    mkSeparatedBy = SeparatedBy . intercalate (pure (sep (Proxy @sep)))


class MkUpperCased str where
    to           :: str -> String
    from         :: String -> str
    mkUpperCased :: str -> UpperCased str
    mkUpperCased = UpperCased . map C.toUpper . to


someWords :: [String]
someWords =
    [ "bulbasaur"
    , "charmander"
    , "squirtle"
    , "caterpie"
    , "weedle"
    , "pidgey"
    , "rattata"
    , "spearow"
    , "ekans"
    , "sandshrew"
    ]
