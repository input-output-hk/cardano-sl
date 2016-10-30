{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Server part.

module Pos.Communication.Server
       ( allListeners
       , module SysStart
       , module Statistics
       ) where

import           Universum

import           Control.TimeWarp.Rpc                (BinaryP, MonadDialog)
import           Pos.Communication.Server.Block      (blockListeners)
import           Pos.Communication.Server.Mpc        (mpcListeners)
import           Pos.Communication.Server.Statistics as Statistics
import           Pos.Communication.Server.SysStart   as SysStart
import           Pos.Communication.Server.Tx         (txListeners)
import           Pos.DHT                             (ListenerDHT)
import           Pos.WorkMode                        (WorkMode)

allListeners :: (MonadDialog BinaryP m, WorkMode m) => [ListenerDHT m]
allListeners = mconcat [blockListeners, mpcListeners, txListeners]
