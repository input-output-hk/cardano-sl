{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Server part.

module Pos.Communication.Server
       ( allListeners
       , serverLoggerName
       , module SysStart
       , module Statistics
       ) where

import           Control.TimeWarp.Logging            (LoggerName)
import           Control.TimeWarp.Rpc                (BinaryP, MonadDialog)
import           Data.Tagged                         (untag)
import           Universum

import           Pos.Communication.Server.Block      (blockListeners)
import           Pos.Communication.Server.Mpc        ()
import           Pos.Communication.Server.Statistics as Statistics
import           Pos.Communication.Server.SysStart   as SysStart
import           Pos.Communication.Server.Tx         (txListeners)
import           Pos.Communication.Util              (modifyListenerLogger)
import           Pos.DHT                             (ListenerDHT)
import           Pos.Ssc.Class.Listeners             (sscListeners)
import           Pos.Ssc.DynamicState                (SscDynamicState)
import           Pos.WorkMode                        (WorkMode)

allListeners :: (MonadDialog BinaryP m, WorkMode m) => [ListenerDHT m]
allListeners =
    map (modifyListenerLogger serverLoggerName) $
    concat [blockListeners, untag @SscDynamicState sscListeners, txListeners]

serverLoggerName :: LoggerName
serverLoggerName = "server"
