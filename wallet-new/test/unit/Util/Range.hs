{-# LANGUAGE TemplateHaskell #-}

module Util.Range (
    -- * Ranges
    Range(..)
  , Ranges(..)
    -- ** Lenses
  , rangeLo
  , rangeHi
  , xRange
  , yRange
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))

{-------------------------------------------------------------------------------
  Ranges
-------------------------------------------------------------------------------}

-- | Range with a 'Buildable' instance that produces valid gnuplot output
data Range = Range { _rangeLo :: Double, _rangeHi :: Double }

-- | X-range and y-range
data Ranges = Ranges { _xRange :: Range, _yRange :: Range }

makeLenses ''Range
makeLenses ''Ranges

-- | Union two 'Range's
instance Monoid Range where
  mempty      = Range 0 0
  mappend a b = Range {
                    _rangeLo = Universum.min (a ^. rangeLo) (b ^. rangeLo)
                  , _rangeHi = Universum.max (a ^. rangeHi) (b ^. rangeHi)
                  }

-- | Union two 'Ranges'
instance Monoid Ranges where
  mempty      = Ranges mempty mempty
  mappend a b = Ranges {
                    _xRange = mappend (a ^. xRange) (b ^. xRange)
                  , _yRange = mappend (a ^. yRange) (b ^. yRange)
                  }

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

-- | Render range in gnuplot compatible syntax
instance Buildable Range where
  build (Range lo hi) = bprint ("[" % build % ":" % build % "]") lo hi
