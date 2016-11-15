{-# LANGUAGE AllowAmbiguousTypes   #-}
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
import           Pos.Communication.Server.Statistics as Statistics
import           Pos.Communication.Server.SysStart   as SysStart
import           Pos.Communication.Server.Tx         (txListeners)
import           Pos.Communication.Util              (modifyListenerLogger)
import           Pos.DHT                             (ListenerDHT)
import           Pos.Ssc.Class.Listeners             (SscListenersClass, sscListeners)
import           Pos.WorkMode                        (WorkMode)

allListeners :: (SscListenersClass ssc, MonadDialog BinaryP m, WorkMode ssc m)
             => [ListenerDHT m]
allListeners =
    map (modifyListenerLogger serverLoggerName) $
    concat [blockListeners,  untag sscListeners, txListeners]

serverLoggerName :: LoggerName
serverLoggerName = "server"
