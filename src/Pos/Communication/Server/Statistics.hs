{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Listener for stats delivery

module Pos.Communication.Server.Statistics
       ( statsListener
       ) where

import           Control.TimeWarp.Logging  (logInfo)
import           Control.TimeWarp.Rpc      (BinaryP, MonadDialog)
import           Formatting                (sformat, stext, (%))
import           Universum

import           Pos.Communication.Types   (RequestStat (..), ResponseMode,
                                            ResponseStat (..))
import           Pos.DHT                   (ListenerDHT (..), replyToNode)
import           Pos.Statistics.MonadStats (getStats)
import           Pos.WorkMode              (WorkMode)

statsListener :: (MonadDialog BinaryP m, WorkMode m) => ListenerDHT m
statsListener = ListenerDHT handleStatsRequests

handleStatsRequests :: ResponseMode m => RequestStat -> m ()
handleStatsRequests (RequestStat id label) = do
    logInfo $ sformat ("Requested statistical data with label "%stext) label
    stats <- getStats label
    replyToNode $ ResponseStat id label stats
