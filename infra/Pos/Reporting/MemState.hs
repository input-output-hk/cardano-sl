{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

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

instance MonadReportingMem m => MonadReportingMem (ReaderT s m) where
instance MonadReportingMem m => MonadReportingMem (ExceptT s m) where
instance MonadReportingMem m => MonadReportingMem (StateT s m) where
