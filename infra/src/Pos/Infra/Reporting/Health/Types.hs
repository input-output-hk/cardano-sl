-- | Types relevant to health reporting.

module Pos.Infra.Reporting.Health.Types
    ( HealthStatus (..)
    ) where

import           Data.Text (Text)

data HealthStatus = HSHealthy Text | HSUnhealthy Text
