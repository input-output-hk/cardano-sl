{-# LANGUAGE TypeFamilies #-}

module Pos.Reporting.Class
       ( MonadReportingMem (..)
       ) where

import           Control.Monad.Trans (MonadTrans)
import           Universum

class Monad m => MonadReportingMem m where
    askReportingMem :: m [Text]

    default askReportingMem :: (MonadTrans t, MonadReportingMem m', t m' ~ m) =>
       m [Text]
    askReportingMem = lift askReportingMem

instance MonadReportingMem m => MonadReportingMem (ReaderT s m) where
instance MonadReportingMem m => MonadReportingMem (ExceptT s m) where
instance MonadReportingMem m => MonadReportingMem (StateT s m) where
