{-# LANGUAGE TemplateHaskell #-}

-- | Logic of Explorer socket-io Server.

module Pos.Explorer.Web.Sockets.App
       ( notifierApp
       ) where

import           Network.SocketIO                 ()
import           Universum

import           Pos.Explorer.Web.Sockets.Holder  ()
import           Pos.Explorer.Web.Sockets.Methods ()
import           Pos.Explorer.Web.Sockets.Util    ()

notifierApp :: MonadIO m => m ()
notifierApp = return ()
