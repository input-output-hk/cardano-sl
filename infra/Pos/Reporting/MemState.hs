{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}

-- | Reporting functionality abstracted.
module Pos.Reporting.MemState
       ( ReportingContext (..)
       , rcReportServers
       , rcLoggingConfig
       , MonadReportingMem
       , askReportingContext
       , runWithoutReportingContext
       ) where

import           Control.Lens                 (makeLenses)
import qualified Control.Monad.Ether.Implicit as Ether
import           System.Wlog.LoggerConfig     (LoggerConfig)
import           Universum

-- | Context needed to provide remote reporting capabilities.
data ReportingContext = ReportingContext
    { _rcReportServers :: ![Text] -- ^ Report servers list (urls)
    , _rcLoggingConfig :: !LoggerConfig
    }

makeLenses ''ReportingContext

-- | Monads are able to do remote error reporting. IO for making http
-- requests, context for saving reporting-related data.
type MonadReportingMem = Ether.MonadReader ReportingContext

askReportingContext :: MonadReportingMem m => m ReportingContext
askReportingContext = Ether.ask

runWithoutReportingContext :: Ether.ReaderT ReportingContext m a -> m a
runWithoutReportingContext m =
  Ether.runReaderT m $ ReportingContext [] mempty
