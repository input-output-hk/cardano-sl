-- | Communication-specific utility functions.

module Pos.Communication.Util
       ( wrapListener
       , wrapActionSpec
       , wrapSendActions
       ) where

import           Universum

import           Data.Time.Units             (Microsecond)
import           Formatting                  (sformat, shown, (%))
import           Mockable                    (Async, Bracket, Delay, Mockable)
import qualified Node                        as N
import           Serokell.Util.Base16        (base16F)
import           System.Wlog                 (LoggerName, WithLogger, logWarning,
                                              modifyLoggerName)

import           Pos.Communication.BiP       (BiP)
import           Pos.Communication.Constants (networkReceiveTimeout)
import           Pos.Communication.Protocol  (ActionSpec (..), Listener, Message (..),
                                              MessageName (..), PeerData, mapActionSpec,
                                              mapListener, mapListener')
import           Pos.Util.TimeLimit          (CanLogInParallel, execWithTimeLimit,
                                              logWarningWaitLinear)

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
        logWarningWaitLinear 4
          (sformat ("Send "%base16F%" to "%shown%" in conversation")
            sndMsg nodeId) $
           N.send conv msg
    recv' =
        logWarningWaitLinear 4
          (sformat ("Recv from "%shown%" in conversation") nodeId) $
           N.recv conv
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
        logWarningWaitLinear 4
          (sformat ("Send to "%shown%" in conversation") nodeId) $
            N.send conv msg
    recv' =
        logWarningWaitLinear 4
          (sformat ("Recv "%base16F%" from "%shown%" in conversation") rcvMsg nodeId) $
            N.recv conv
    MessageName rcvMsg = messageName $
        ((\_ -> Proxy) :: N.ConversationActions snd rcv m -> Proxy rcv) conv

convWithTimeLimit
    :: (Mockable Async m, Mockable Bracket m, Mockable Delay m, WithLogger m)
    => Microsecond
    -> N.NodeId
    -> N.ConversationActions snd rcv m
    -> N.ConversationActions snd rcv m
convWithTimeLimit timeout nodeId conv = conv { N.recv = recv', N.send = send' }
      where
        send' msg = do
            res <- execWithTimeLimit timeout $ N.send conv msg
            whenNothing res . logWarning $
                sformat ("Send to "%shown%" in conversation - timeout expired")
                    nodeId
        recv' = do
            res <- execWithTimeLimit timeout $ N.recv conv
            case res of
                Nothing -> do
                    logWarning $
                        sformat ("Recv from "%shown%" in conversation - timeout expired")
                            nodeId
                    return Nothing
                Just r  -> return r

sendActionsWithTimeLimit
    :: (Mockable Async m, Mockable Bracket m, Mockable Delay m, WithLogger m)
    => Microsecond
    -> N.SendActions BiP PeerData m
    -> N.SendActions BiP PeerData m
sendActionsWithTimeLimit timeout sendActions = sendActions
    { N.withConnectionTo =
        \nodeId mkConv ->
          N.withConnectionTo sendActions nodeId $ \peerData ->
              case mkConv peerData of
                  N.Conversation l ->
                      N.Conversation $ \cA ->
                          l $ convWithTimeLimit timeout nodeId cA
    }

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
    addTimeout networkReceiveTimeout .
    modifyLogger lname
  where
    addWaitLogging = mapListener' sendActionsWithWaitLog convWithWaitLogL identity
    addTimeout timeout = mapListener' (sendActionsWithTimeLimit timeout)
                                      (convWithTimeLimit timeout) identity
    modifyLogger _name = mapListener $ modifyLoggerName (<> lname)

wrapActionSpec
  :: ( CanLogInParallel m
     , Mockable Bracket m
     )
  => LoggerName -> ActionSpec m a -> ActionSpec m a
wrapActionSpec lname =
    addWaitLogging .
    addTimeout networkReceiveTimeout .
    modifyLogger lname
  where
    addWaitLogging = mapActionSpec sendActionsWithWaitLog identity
    addTimeout timeout = mapActionSpec (sendActionsWithTimeLimit timeout) identity
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
    sendActionsWithWaitLog .
    sendActionsWithTimeLimit networkReceiveTimeout
