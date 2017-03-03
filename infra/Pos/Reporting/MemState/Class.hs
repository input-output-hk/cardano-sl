{-# LANGUAGE TypeFamilies #-}

module Pos.Reporting.MemState.Class
       ( MonadReportingMem (..)
       ) where

import           Control.Monad.Trans          (MonadTrans)
import           Pos.Reporting.MemState.Types (ReportingContext)
import           Universum

class Monad m => MonadReportingMem m where
    askReportingMem :: m ReportingContext

    default askReportingMem :: (MonadTrans t, MonadReportingMem m', t m' ~ m) =>
       m ReportingContext
    askReportingMem = lift askReportingMem

instance MonadReportingMem m => MonadReportingMem (ReaderT s m) where
instance MonadReportingMem m => MonadReportingMem (ExceptT s m) where
instance MonadReportingMem m => MonadReportingMem (StateT s m) where
