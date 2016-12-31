{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Datatypes which serve as stat data

module Pos.Statistics.StatEntry
       ( StatEntry
       , StatLabel (..)
       , CountStat (..)
       , ValueStat (..)
       ) where

import           Data.Binary         (Binary)
import           Data.Hashable       (Hashable)
import           Data.SafeCopy       (base, deriveSafeCopySimple)
import           Data.Text.Buildable (Buildable (..))
import           Universum

import           Pos.Util.JsonLog    (JLEvent)

type FullySerializable s = (Binary s, Typeable s)

-- | Stat entry is a simple counter or a structure for aggregating
-- statistical data about real value
type StatEntry e = (Monoid e, FullySerializable e)

-- | `StatLabel` is some datatype which determines a single stat
class (FullySerializable l, Buildable l, Hashable l, StatEntry (EntryType l)) =>
      StatLabel l  where
    type EntryType l :: *
    labelName :: Proxy l -> Text
    toJLEvent :: l -> EntryType l -> JLEvent

-- | Counter for specified statistics.
newtype CountStat = CountStat
    { getCounter :: Word64
    } deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Generic, Binary, Typeable)

-- | Value for specified collected statistic.
data ValueStat = ValueStat
    { valueCount :: !Word64
    , valueMin   :: !Double
    , valueMax   :: !Double
    , valueSum   :: !Double
    } deriving (Show, Eq, Generic, Typeable)

instance Monoid CountStat where
    mappend = (+)
    mempty = 0

instance Monoid ValueStat where
    mappend a b = ValueStat
        { valueCount = valueCount a + valueCount b
        , valueMin = valueMin a `min` valueMin b
        , valueMax = valueMax a `max` valueMax b
        , valueSum = valueSum a + valueSum b
        }
    mempty = ValueStat
        { valueCount = 0
        , valueMin = 1.0/0.0     -- Infinity
        , valueMax = -(1.0/0.0)  -- -Infinity
        , valueSum = 0
        }

-- | Instances for acid and network

instance Binary ValueStat

deriveSafeCopySimple 0 'base ''CountStat
deriveSafeCopySimple 0 'base ''ValueStat
