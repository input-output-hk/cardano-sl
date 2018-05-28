{-# LANGUAGE DeriveFunctor #-}

-- | Random distributions
module Util.Distr (
    Distribution(..)
  , drawFromDistr'
    -- * Basic distributions
  , ConstDistr(..)
  , UniformDistr(..)
  , NormalDistr(..)
    -- * Distribution combinators
  , FreqDistr
  ) where

import           Universum

import           Data.Random.Normal (normal)
import           System.Random
import           Test.QuickCheck
import           Test.QuickCheck.Gen (chooseAny)

{-------------------------------------------------------------------------------
  Distributions
-------------------------------------------------------------------------------}

class Functor f => Distribution f where
  drawFromDistr :: (Floating a, Random a) => f a -> Gen a

-- | Variation on 'drawFromDistr' for integer values.
drawFromDistr' :: forall f a. (Distribution f, Integral a) => f a -> Gen a
drawFromDistr' distr =
    aux <$> drawFromDistr (fromIntegral <$> distr)
  where
    aux :: Double -> a
    aux = round

{-------------------------------------------------------------------------------
  Basic distributions
-------------------------------------------------------------------------------}

-- | Constant distribution
data ConstDistr a = ConstDistr {
      constDistrValue :: a
    }
  deriving (Functor)

-- | Uniform distribution given by lower and upper (inclusive) bounds
data UniformDistr a = UniformDistr {
      uniformDistrLo :: a
    , uniformDistrHi :: a
    }
  deriving (Functor)

-- | Normal distribution
data NormalDistr a = NormalDistr {
      normalDistrMean   :: a
    , normalDistrStdDev :: a
    }
  deriving (Functor)

instance Distribution ConstDistr where
  drawFromDistr = return . constDistrValue

instance Distribution UniformDistr where
  drawFromDistr (UniformDistr lo hi) = choose (lo, hi)

instance Distribution NormalDistr where
  drawFromDistr :: forall a. (Floating a, Random a) => NormalDistr a -> Gen a
  drawFromDistr (NormalDistr mean sigma) = aux <$> chooseAny
    where
      aux :: Normal a -> a
      aux (Normal x) = x * sigma + mean

{-------------------------------------------------------------------------------
  Distribution combinators
-------------------------------------------------------------------------------}

-- | Draw from a bunch of distributions with the specified frequencies
newtype FreqDistr f a = FreqDistr [(Int, f a)]
  deriving (Functor)

instance Distribution f => Distribution (FreqDistr f) where
  drawFromDistr (FreqDistr ds) = frequency $ map (second drawFromDistr) ds

{-------------------------------------------------------------------------------
  Auxiliary: primitives for working with normal distributions
-------------------------------------------------------------------------------}

newtype Normal a = Normal a

instance (Floating a, Random a) => Random (Normal a) where
  random  = (\ (x, g) -> (Normal x, g)) . normal
  randomR = error "randomR not defined for Normal"

instance (Floating a, Random a) => Arbitrary (Normal a) where
  arbitrary = chooseAny
