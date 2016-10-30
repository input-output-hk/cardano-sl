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
import           Universum

import           Pos.Communication.Server.Block      (blockListeners)
import           Pos.Communication.Server.Mpc        (mpcListeners)
import           Pos.Communication.Server.Statistics as Statistics
import           Pos.Communication.Server.SysStart   as SysStart
import           Pos.Communication.Server.Tx         (txListeners)
import           Pos.Communication.Util              (modifyListenerLogger)
import           Pos.DHT                             (ListenerDHT)
import           Pos.WorkMode                        (WorkMode)

allListeners :: (MonadDialog BinaryP m, WorkMode m) => [ListenerDHT m]
allListeners =
    map (modifyListenerLogger serverLoggerName) $
    concat [blockListeners, mpcListeners, txListeners]

serverLoggerName :: LoggerName
serverLoggerName = "server"
