module Pos.Reporting.Class
       ( MonadReportingMem (..)
       ) where

import           Universum

class MonadReportingMem m where
    askReportingMem :: m [Text]
