{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Server which deals with blocks processing.

module Pos.Modern.Blockp.Server.Listeners
       ( blockListeners
       ) where

import           Universum

import           Pos.Binary.Communication ()
import           Pos.Communication.Types  (MutSocketState, ResponseMode, SendBlock (..))
import           Pos.DHT.Model            (ListenerDHT (..), MonadDHTDialog)
import           Pos.WorkMode             (WorkMode)

-- | Listeners for requests related to blocks processing.
blockListeners
    :: (MonadDHTDialog (MutSocketState ssc) m, WorkMode ssc m)
    => [ListenerDHT (MutSocketState ssc) m]
blockListeners =
    [ ListenerDHT handleBlock
    ]

handleBlock
    :: forall ssc m.
       (ResponseMode ssc m)
    => SendBlock ssc -> m ()
handleBlock (SendBlock _) = pass
