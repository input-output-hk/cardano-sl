{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Reporting functionality abstracted.
module Pos.Reporting.MemState
       ( ReportingContext (..)
       , rcReportServers
       , rcLoggingConfig
       , MonadReportingMem (..)
       ) where

import           Control.Lens             (makeLenses)
import           Control.Monad.Trans      (MonadTrans)
import           System.Wlog.LoggerConfig (LoggerConfig)
import           Universum

-- | Context needed to provide remote reporting capabilities.
data ReportingContext = ReportingContext
    { _rcReportServers :: ![Text] -- ^ Report servers list (urls)
    , _rcLoggingConfig :: !LoggerConfig
    }

makeLenses ''ReportingContext

-- | Monads are able to do remote error reporting. IO for making http
-- requests, context for saving reporting-related data.
class (Monad m) => MonadReportingMem m where
    askReportingContext :: m ReportingContext

    default askReportingContext :: (MonadTrans t, MonadReportingMem m', t m' ~ m) =>
       m ReportingContext
    askReportingContext = lift askReportingContext

instance {-# OVERLAPPABLE #-}
  (MonadReportingMem m, MonadTrans t, Monad (t m)) =>
  MonadReportingMem (t m)
