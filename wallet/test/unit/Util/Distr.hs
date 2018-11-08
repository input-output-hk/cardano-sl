{-# LANGUAGE DeriveFunctor #-}

-- | Random distributions
module Util.Distr (
    Distribution(..)
  , drawFromDistr'
    -- * Basic distributions
  , Constant(..)
  , Uniform(..)
  , Normal(..)
  , Exponential(..)
  , Erlang(..)
  , Gumbel(..)
    -- * Distribution combinators
  , FreqDistr
  ) where

import           Prelude (log)
import           Universum

import           Data.Random.Normal (normal)
import           System.Random
import           Test.QuickCheck
import           Test.QuickCheck.Gen (chooseAny)

{-------------------------------------------------------------------------------
  Distributions
-------------------------------------------------------------------------------}

class Distribution d where
  drawFromDistr :: d -> Gen Double

-- | Variation on 'drawFromDistr' for integer values.
drawFromDistr' :: forall d a. (Distribution d, Integral a)  => d -> Gen a
drawFromDistr' = fmap round . drawFromDistr

{-------------------------------------------------------------------------------
  Basic distributions
-------------------------------------------------------------------------------}

-- | Constant distribution
data Constant =  Constant {
      constDistrValue :: Double
    }

-- | Uniform distribution given by lower and upper (inclusive) bounds
data Uniform = Uniform {
      uniformDistrLo :: Double
    , uniformDistrHi :: Double
    }

-- | Normal distribution
data Normal = Normal {
      normalDistrMean   :: Double
    , normalDistrStdDev :: Double
    }

-- | Exponential distribution
--
-- <https://en.wikipedia.org/wiki/Exponential_distribution>
data Exponential = Exponential {
      expDistrRate :: Double
    }

-- | Gumbel distribution
--
-- <https://en.wikipedia.org/wiki/Gumbel_distribution>
data Gumbel = Gumbel {
      gumbelLocation :: Double -- ^ mu
    , gumbelScale    :: Double -- ^ beta
    }

-- | Erlang distribution
--
-- <https://en.wikipedia.org/wiki/Erlang_distribution>
data Erlang = Erlang {
      erlangShape :: Int
    , erlangRate  :: Double
    }

instance Distribution Constant where
  drawFromDistr = return . constDistrValue

instance Distribution Uniform where
  drawFromDistr (Uniform lo hi) = choose (lo, hi)

instance Distribution Normal where
  drawFromDistr (Normal mean sigma) =
      aux <$> chooseAny
    where
      aux (FromNormal x) = x * sigma + mean

-- | See <https://en.wikipedia.org/wiki/Exponential_distribution#Generating_exponential_variates>
--
-- NOTE: Wikipedia specifies
--
-- > T = (-1 * ln U) / lambda
--
-- but
--
-- >    (-1 * ln U) / lambda
-- > == (-1 / lambda) * ln U
--
-- We can thus immediately see that this is equal to an erlang distribution
-- with @k = 1@.
instance Distribution Exponential where
  drawFromDistr (Exponential lambda) = drawFromDistr (Erlang 1 lambda)

-- | See <https://en.wikipedia.org/wiki/Erlang_distribution#Generating_Erlang-distributed_random_variates>
instance Distribution Erlang where
  drawFromDistr (Erlang k lambda) =
      aux <$> replicateM k (choose (0, 1) `suchThat` (/= 0))
    where
      aux us = (-1 / lambda) * log (product us)

-- | See <https://en.wikipedia.org/wiki/Gumbel_distribution#Quantile_function_and_generating_Gumbel_variates>
instance Distribution Gumbel where
  drawFromDistr (Gumbel mu beta) =
      aux <$> choose (0, 1) `suchThat` (/= 0)
    where
      aux u = mu - beta * log(-1 * log u)

{-------------------------------------------------------------------------------
  Distribution combinators
-------------------------------------------------------------------------------}

-- | Draw from a bunch of distributions with the specified frequencies
newtype FreqDistr d = FreqDistr [(Int, d)]
  deriving (Functor)

instance Distribution d => Distribution (FreqDistr d) where
  drawFromDistr (FreqDistr ds) = frequency $ map (second drawFromDistr) ds

{-------------------------------------------------------------------------------
  Auxiliary: primitives for working with normal distributions
-------------------------------------------------------------------------------}

newtype FromNormal a = FromNormal a

instance (Floating a, Random a) => Random (FromNormal a) where
  random  = (\ (x, g) -> (FromNormal x, g)) . normal
  randomR = error "randomR not defined for FromNormal"

instance (Floating a, Random a) => Arbitrary (FromNormal a) where
  arbitrary = chooseAny
