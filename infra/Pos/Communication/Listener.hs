{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Protocol/versioning related communication helpers.

module Pos.Communication.Listener
       ( listenerConv
       , SizedCAHandler (..)
       , convToSProxy
       ) where

import           Data.Proxy                     (asProxyTypeOf)
import           Data.Reflection                (Reifies)
import qualified Node                           as N
import           System.Wlog                    (WithLogger)
import           Universum

import           Pos.Binary.Class               (Bi)
import           Pos.Binary.Infra               ()
import           Pos.Communication.Limits.Types (LimitType, MessageLimited, SmartLimit,
                                                 reifyMsgLimit)
import           Pos.Communication.Protocol     (ConversationActions, HandlerSpec (..),
                                                 ListenerSpec (..), Message, NodeId,
                                                 OutSpecs, VerInfo, checkingInSpecs,
                                                 messageName)
import           Pos.DB.Class                   (MonadGStateCore)


data SizedCAHandler snd rcv m =
    SizedCAHandler
      (forall s. Reifies s (LimitType rcv) =>
            NodeId -> ConversationActions snd (SmartLimit s rcv) m -> m ())

convToSProxy :: ConversationActions snd (SmartLimit s rcv) m -> Proxy s
convToSProxy _ = Proxy

listenerConv
    :: ( Bi snd
       , Bi rcv
       , Message snd
       , Message rcv
       , MonadGStateCore m
       , MessageLimited rcv
       , WithLogger m
       )
    => (VerInfo -> SizedCAHandler snd rcv m)
    -> (ListenerSpec m, OutSpecs)
listenerConv h = (lspec, mempty)
  where
    spec = (rcvMsgName, ConvHandler sndMsgName)
    lspec =
      flip ListenerSpec spec $ \ourVerInfo ->
      reifyMsgLimit rcvProxy $ \(_ :: Proxy s) ->
          let convProxy = (const Proxy :: (a -> SizedCAHandler snd rcv m)
                           -> Proxy (ConversationActions snd (SmartLimit s rcv) m)) h
          in case h ourVerInfo of
              SizedCAHandler handle ->
                  pure $ N.ListenerActionConversation $ \peerVerInfo' nNodeId conv -> do
                      let _ = conv `asProxyTypeOf` convProxy
                      checkingInSpecs ourVerInfo peerVerInfo' spec nNodeId $
                          handle @s nNodeId conv

    sndProxy = (const Proxy :: (a -> SizedCAHandler snd rcv m) -> Proxy snd) h
    rcvProxy = (const Proxy :: (a -> SizedCAHandler snd rcv m) -> Proxy rcv) h

    sndMsgName = messageName sndProxy
    rcvMsgName = messageName rcvProxy
