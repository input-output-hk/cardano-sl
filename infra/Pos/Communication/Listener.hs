{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Protocol/versioning related communication helpers.

module Pos.Communication.Listener
       ( listenerConv
       ) where

import qualified Node                           as N
import           System.Wlog                    (WithLogger)
import           Universum

import           Mockable.Class                 (Mockable)
import           Mockable.Exception             (Throw)
import           Pos.Binary.Class               (Bi)
import           Pos.Binary.Infra               ()
import           Pos.Communication.Limits.Types (MessageLimited)
import           Pos.Communication.Protocol     (ConversationActions, HandlerSpec (..),
                                                 ListenerSpec (..), Message, NodeId,
                                                 OutSpecs, VerInfo, checkingInSpecs,
                                                 messageCode)
import           Pos.DB.Class                   (MonadGState)

-- TODO automatically provide a 'recvLimited' here by using the
-- 'MessageLimited'?
listenerConv
    :: forall snd rcv m .
       ( Bi snd
       , Bi rcv
       , Message snd
       , Message rcv
       , MonadGState m
       , MessageLimited rcv
       , WithLogger m
       , Mockable Throw m
       )
    => (VerInfo -> NodeId -> ConversationActions snd rcv m -> m ())
    -> (ListenerSpec m, OutSpecs)
listenerConv h = (lspec, mempty)
  where
    spec = (rcvMsgCode, ConvHandler sndMsgCode)
    lspec =
      flip ListenerSpec spec $ \ourVerInfo ->
          N.Listener $ \peerVerInfo' nNodeId conv -> do
              checkingInSpecs ourVerInfo peerVerInfo' spec nNodeId $
                  h ourVerInfo nNodeId conv

    sndProxy :: Proxy snd
    sndProxy = Proxy
    rcvProxy :: Proxy rcv
    rcvProxy = Proxy

    sndMsgCode = messageCode sndProxy
    rcvMsgCode = messageCode rcvProxy
