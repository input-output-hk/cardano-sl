-- | Communication-specific utility functions.

module Pos.Communication.Util
       ( modifyListenerLogger
       , stubListenerOneMsg
       , stubListenerConv
       , withWaitLogConv
       , withWaitLogConvL
       , withWaitLog
       , convWithTimeLimit
       , sendActionsWithTimeLimit
       ) where

import           Data.Proxy                 (Proxy (..), asProxyTypeOf)
import           Data.Time.Units            (Microsecond)
import           Formatting                 (build, sformat, shown, (%))
import           Mockable                   (Async, Bracket, Delay, Mockable)
import qualified Node                       as N
import           System.Wlog                (HasLoggerName, LoggerName, WithLogger,
                                             logDebug, logWarning, modifyLoggerName)
import           Universum                  hiding (Async, async, bracket, cancel,
                                             finally)

import           Pos.Binary.Class           (Bi)
import           Pos.Communication.BiP      (BiP)
import           Pos.Communication.Protocol (ConversationActions, HandlerSpec (..),
                                             Listener, ListenerSpec (..), Message (..),
                                             MessageName (..), OutSpecs, PeerData,
                                             listenerConv, listenerOneMsg, mapListener,
                                             messageName')
import           Pos.Util.TimeLimit         (CanLogInParallel, execWithTimeLimit,
                                             logWarningWaitLinear)

-- | Append given logger name to the name used by listener.
modifyListenerLogger
    :: (HasLoggerName m)
    => LoggerName -> Listener m -> Listener m
modifyListenerLogger name = mapListener $ modifyLoggerName (<> name)

stubListenerOneMsg
    :: (WithLogger m, Message r, Bi r)
    => Proxy r -> (ListenerSpec m, OutSpecs)
stubListenerOneMsg p = (ListenerSpec listener (rcvName, OneMsgHandler), mempty)
  where
    rcvName = messageName p
    listener _ = N.ListenerActionOneMsg $
      \_d __nId __sA m ->
        let _ = m `asProxyTypeOf` p
         in modifyLoggerName (<> "stub") $
              logDebug $ sformat
                  ("Stub listener (one msg) for "%build%": received message")
                  rcvName

stubListenerConv
    :: (WithLogger m, Message snd, Message rcv, Bi rcv, Bi snd)
    => Proxy (rcv, snd) -> (ListenerSpec m, OutSpecs)
stubListenerConv p = (ListenerSpec listener (rcvName, ConvHandler sndName), mempty)
  where
    modP :: Proxy (rcv, snd) -> Proxy (N.ConversationActions PeerData snd rcv m)
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

withWaitLog :: ( CanLogInParallel m )
            => N.SendActions BiP PeerData m
            -> N.SendActions BiP PeerData m
withWaitLog sendActions = sendActions
    { N.sendTo = \nodeId msg ->
                  let MessageName mName = messageName' msg
                   in logWarningWaitLinear 4
                        (sformat ("Send "%shown%" to "%shown) mName nodeId) $
                          N.sendTo sendActions nodeId msg
    , N.withConnectionTo =
        \nodeId action ->
          N.withConnectionTo sendActions nodeId $
              action . withWaitLogConv nodeId
    }

withWaitLogConv
    :: (CanLogInParallel m, Message snd)
    => N.NodeId
    -> N.ConversationActions PeerData snd rcv m
    -> N.ConversationActions PeerData snd rcv m
withWaitLogConv nodeId conv = conv { N.send = send', N.recv = recv' }
  where
    send' msg =
        logWarningWaitLinear 4
          (sformat ("Send "%shown%" to "%shown%" in conversation") sndMsg nodeId) $
            N.send conv msg
    recv' =
        logWarningWaitLinear 4
          (sformat ("Recv from "%shown%" in conversation") nodeId) $
           N.recv conv
    MessageName sndMsg = messageName $
        ((\_ -> Proxy) :: N.ConversationActions PeerData snd rcv m -> Proxy snd) conv

withWaitLogConvL
    :: (CanLogInParallel m, Message rcv)
    => N.NodeId
    -> N.ConversationActions PeerData snd rcv m
    -> N.ConversationActions PeerData snd rcv m
withWaitLogConvL nodeId conv = conv { N.send = send', N.recv = recv' }
  where
    send' msg =
        logWarningWaitLinear 4
          (sformat ("Send to "%shown%" in conversation") nodeId) $
            N.send conv msg
    recv' =
        logWarningWaitLinear 4
          (sformat ("Recv "%shown%" from "%shown%" in conversation") rcvMsg nodeId) $
            N.recv conv
    MessageName rcvMsg = messageName $
        ((\_ -> Proxy) :: N.ConversationActions PeerData snd rcv m -> Proxy rcv) conv

convWithTimeLimit
    :: (Mockable Async m, Mockable Bracket m, Mockable Delay m, WithLogger m)
    => Microsecond
    -> N.NodeId
    -> N.ConversationActions PeerData snd rcv m
    -> N.ConversationActions PeerData snd rcv m
convWithTimeLimit timeout nodeId conv = conv { N.recv = recv' }
      where
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
    { N.withConnectionTo = \nodeId action ->
        N.withConnectionTo sendActions nodeId $
            action . convWithTimeLimit timeout nodeId
    }
