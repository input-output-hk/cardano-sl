-- | Communication-specific utility functions.

module Pos.Communication.Util
       ( stubListenerConv
       , wrapListener
       , wrapActionSpec
       , wrapSendActions
       ) where

import           Universum

import           Data.Proxy                  (asProxyTypeOf)
import           Data.Time.Units             (Microsecond)
import           Formatting                  (build, sformat, shown, (%))
import           Mockable                    (Async, Bracket, Delay, Mockable)
import qualified Node                        as N
import           Serokell.Util.Base16        (base16F)
import           System.Wlog                 (LoggerName, WithLogger, logDebug,
                                              logWarning, modifyLoggerName)

import           Pos.Binary.Class            (Bi)
import           Pos.Communication.BiP       (BiP)
import           Pos.Communication.Constants (networkReceiveTimeout)
import           Pos.Communication.Protocol  (ActionSpec (..), HandlerSpec (..), Listener,
                                              ListenerSpec (..), Message (..),
                                              MessageName (..), OutSpecs, PeerData,
                                              mapActionSpec, mapListener, mapListener')
import           Pos.Util.TimeLimit          (CanLogInParallel, execWithTimeLimit,
                                              logWarningWaitLinear)

stubListenerConv
    :: (WithLogger m, Message snd, Message rcv, Bi rcv, Bi snd)
    => Proxy (rcv, snd) -> (ListenerSpec m, OutSpecs)
stubListenerConv p = (ListenerSpec listener (rcvName, ConvHandler sndName), mempty)
  where
    modP :: Proxy (rcv, snd) -> Proxy (N.ConversationActions snd rcv m)
    modP _ = Proxy
    sndProxy :: Proxy (rcv, snd) -> Proxy snd
    sndProxy _ = Proxy
    rcvProxy :: Proxy (rcv, snd) -> Proxy rcv
    rcvProxy _ = Proxy
    rcvName = messageName $ rcvProxy p
    sndName = messageName $ sndProxy p
    listener _ = N.ListenerActionConversation $
      \_d __nId convActions ->
          let _ = convActions `asProxyTypeOf` modP p
           in modifyLoggerName (<> "stub") $
                logDebug $ sformat
                    ("Stub listener ("%build%", Conv "%build%"): received message")
                    rcvName
                    sndName

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
     , Mockable Async m
     , Mockable Bracket m
     , Mockable Delay m
     , MonadIO m
     , WithLogger m
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
