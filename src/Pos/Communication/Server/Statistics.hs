{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

-- | Listener for stats delivery

module Pos.Communication.Server.Statistics
       ( statsListeners
       ) where

import           Control.TimeWarp.Logging  (logInfo)
import           Control.TimeWarp.Rpc      (BinaryP, MonadDialog)
import           Formatting                (build, sformat, (%))
import           Universum

import           Pos.Communication.Types   (RequestStat (..), ResponseMode,
                                            ResponseStat (..))
import           Pos.DHT                   (ListenerDHT (..), replyToNode)
import           Pos.Statistics.Block      (StatBlockCreated)
import           Pos.Statistics.MonadStats (getStats)
import           Pos.Statistics.StatEntry  (StatLabel (..))
import           Pos.Statistics.Tx         (StatProcessTx)
import           Pos.WorkMode              (WorkMode)

statsListeners :: (MonadDialog BinaryP m, WorkMode m) => [ListenerDHT m]
statsListeners = [ ListenerDHT $ handleStatsRequests @StatBlockCreated
                 , ListenerDHT $ handleStatsRequests @StatProcessTx
                 ]

handleStatsRequests :: (StatLabel l, ResponseMode m) => RequestStat l -> m ()
handleStatsRequests (RequestStat id label) = do
    logInfo $ sformat ("Requested statistical data with label "%build) label
    stats <- getStats label
    replyToNode $ ResponseStat id label stats
