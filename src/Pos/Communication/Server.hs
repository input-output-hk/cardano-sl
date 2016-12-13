{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Server part.

module Pos.Communication.Server
       ( allListeners
       , serverLoggerName
       , module Pos.Communication.Server.SysStart
       ) where

import           Data.Tagged                       (untag)
import           System.Wlog                       (LoggerName)
import           Universum

import           Pos.Binary.Communication          ()
import           Pos.Communication.Server.Block    (blockListeners)
import           Pos.Communication.Server.SysStart
import           Pos.Communication.Util            (modifyListenerLogger)
import           Pos.DHT                           (ListenerDHT)
import           Pos.Ssc.Class.Listeners           (SscListenersClass, sscListeners)
import           Pos.Txp.Listeners                 (txListeners)
import           Pos.WorkMode                      (MonadUserDialog, WorkMode)

-- | All listeners running on one node.
allListeners
    :: (SscListenersClass ssc, MonadUserDialog m, WorkMode ssc m)
    => [ListenerDHT m]
allListeners =
    map (modifyListenerLogger serverLoggerName) $
    concat
        [ map (modifyListenerLogger "block") blockListeners
        , map (modifyListenerLogger "ssc") $ untag sscListeners
        , map (modifyListenerLogger "tx") txListeners
        ]

-- | Logger name for server.
serverLoggerName :: LoggerName
serverLoggerName = "server"
