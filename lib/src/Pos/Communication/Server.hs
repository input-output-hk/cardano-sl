-- | Server part.

module Pos.Communication.Server
       ( serverLoggerName
       ) where

import           System.Wlog (LoggerName)

-- | Logger name for server.
serverLoggerName :: LoggerName
serverLoggerName = "server"
