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

import           Pos.Binary.Class                  (Bi)
import           Pos.Communication.Server.Block    (blockListeners)
import           Pos.Communication.Server.SysStart
import           Pos.Communication.Types.Block     as Bl
import           Pos.Communication.Util            (modifyListenerLogger)
import           Pos.DHT                           (ListenerDHT, MonadDHTDialog)
import           Pos.Ssc.Class.Listeners           (SscListenersClass, sscListeners)
import           Pos.Txp.Listeners                 (txListeners)
import           Pos.Txp.Types.Communication       as Txp
import           Pos.WorkMode                      (SocketState, WorkMode)

-- | All listeners running on one node.
allListeners
    :: (SscListenersClass ssc
       ,MonadDHTDialog SocketState m
       ,WorkMode ssc m
       ,Bi (Bl.SendBlock ssc)
       ,Bi (Bl.SendBlockHeader ssc)
       ,Bi (Bl.RequestBlock ssc)
       ,Bi (Bl.RequestBlockchainPart ssc)
       ,Bi (Bl.SendBlockchainPart ssc)
       ,Bi Txp.TxInvMsg
       ,Bi Txp.TxReqMsg
       ,Bi Txp.TxDataMsg)
    => [ListenerDHT SocketState m]
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
