-- | Communication-specific utility functions.

module Pos.Communication.Util
       ( wrapListener
       , wrapActionSpec
       , wrapSendActions
       ) where

import           Universum

import           Formatting                  (sformat, shown, (%))
import           Mockable                    (Async, Bracket, Delay, Mockable)
import qualified Node                        as N
import           Serokell.Util.Base16        (base16F)
import           System.Wlog                 (LoggerName, WithLogger, modifyLoggerName)

import           Pos.Communication.BiP       (BiP)
import           Pos.Communication.Constants (networkWaitLogInterval)
import           Pos.Communication.Protocol  (ActionSpec (..), Listener, Message (..),
                                              MessageName (..), PeerData, mapActionSpec,
                                              mapListener, mapListener')
import           Pos.Util.TimeLimit          (CanLogInParallel, logWarningWaitLinear)

sendActionsWithWaitLog :: ( CanLogInParallel m )
            => N.SendActions BiP PeerData m
            -> N.SendActions BiP PeerData m
sendActionsWithWaitLog sendActions = sendActions
    { N.withConnectionTo =
        \nodeId mkConv ->
          N.withConnectionTo sendActions nodeId $ \peerData ->
              case mkConv peerData of
                  N.Conversation l ->
                      N.Conversation $ \cA ->
                          l $ convWithWaitLog nodeId cA
    }

convWithWaitLog
    :: (CanLogInParallel m, Message snd)
    => N.NodeId
    -> N.ConversationActions snd rcv m
    -> N.ConversationActions snd rcv m
convWithWaitLog nodeId conv = conv { N.send = send', N.recv = recv' }
  where
    send' msg =
        logWarningWaitLinear networkWaitLogInterval
          (sformat ("Send "%base16F%" to "%shown%" in conversation")
            sndMsg nodeId) $
           N.send conv msg
    recv' limit =
        logWarningWaitLinear networkWaitLogInterval
          (sformat ("Recv from "%shown%" in conversation") nodeId) $
           N.recv conv limit
    MessageName sndMsg = messageName $
        ((\_ -> Proxy) :: N.ConversationActions snd rcv m -> Proxy snd) conv

convWithWaitLogL
    :: (CanLogInParallel m, Message rcv)
    => N.NodeId
    -> N.ConversationActions snd rcv m
    -> N.ConversationActions snd rcv m
convWithWaitLogL nodeId conv = conv { N.send = send', N.recv = recv' }
  where
    send' msg =
        logWarningWaitLinear networkWaitLogInterval
          (sformat ("Send to "%shown%" in conversation") nodeId) $
            N.send conv msg
    recv' limit =
        logWarningWaitLinear networkWaitLogInterval
          (sformat ("Recv "%base16F%" from "%shown%" in conversation") rcvMsg nodeId) $
            N.recv conv limit
    MessageName rcvMsg = messageName $
        ((\_ -> Proxy) :: N.ConversationActions snd rcv m -> Proxy rcv) conv

wrapListener
  :: ( CanLogInParallel m
     , Mockable Async m
     , Mockable Bracket m
     , Mockable Delay m
     , MonadIO m
     , WithLogger m
     )
  => LoggerName -> Listener m -> Listener m
wrapListener lname =
    addWaitLogging .
    modifyLogger lname
  where
    addWaitLogging = mapListener' sendActionsWithWaitLog convWithWaitLogL identity
    modifyLogger _name = mapListener $ modifyLoggerName (<> lname)

wrapActionSpec
  :: ( CanLogInParallel m
     , Mockable Bracket m
     )
  => LoggerName -> ActionSpec m a -> ActionSpec m a
wrapActionSpec lname =
    addWaitLogging .
    modifyLogger lname
  where
    addWaitLogging = mapActionSpec sendActionsWithWaitLog identity
    modifyLogger _name = mapActionSpec identity $ modifyLoggerName
                                    (<> lname)

wrapSendActions
  :: ( CanLogInParallel m
     , Mockable Async m
     , Mockable Bracket m
     , Mockable Delay m
     , MonadIO m
     , WithLogger m
     )
  => N.SendActions BiP PeerData m
  -> N.SendActions BiP PeerData m
wrapSendActions =
    sendActionsWithWaitLog
