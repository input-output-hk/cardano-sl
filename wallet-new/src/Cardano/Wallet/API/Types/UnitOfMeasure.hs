
module Cardano.Wallet.API.Types.UnitOfMeasure where

import           Universum

-- | A finite sum type representing time units we might want to show to
-- clients. The idea is that whenever we have a quantity represeting some
-- form of time, we should render it together with the relevant unit, to
-- not leave anything to guessing.
data UnitOfMeasure =
      Seconds
    | Milliseconds
    | Microseconds
    -- | % ranging from 0 to 100.
    | Percentage100
    -- | Number of blocks.
    | Blocks
    -- | Number of blocks per second.
    | BlocksPerSecond
    deriving (Show, Eq)

data MeasuredIn (a :: UnitOfMeasure) b = MeasuredIn b deriving (Eq, Show)
