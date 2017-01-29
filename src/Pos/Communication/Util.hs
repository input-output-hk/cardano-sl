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
import           Formatting                 (sformat, shown, (%))
import           Mockable                   (Async, Bracket, Delay, Fork, Mockable, Throw,
                                             async, bracket, cancel, delay, finally, fork,
                                             killThread, throw, waitAny)
import qualified Node                       as N
import           System.Wlog                (HasLoggerName, LoggerName, WithLogger,
                                             logDebug, logWarning, modifyLoggerName)
import           Universum                  hiding (Async, async, bracket, cancel,
                                             finally)

import           Pos.Binary.Class           (Bi)
import           Pos.Communication.BiP      (BiP)
import           Pos.Communication.Protocol (ConversationActions, Listener, ListenerSpec,
                                             Message (..), MessageName (..), OutSpecs,
                                             PeerData, listenerConv, listenerOneMsg,
                                             mapListener, messageName')
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
stubListenerOneMsg p = listenerOneMsg mempty $ \_d _ _ m ->
                          let _ = m `asProxyTypeOf` p
                           in modifyLoggerName (<> "stub") $
                                logDebug $ sformat
                                    ("Stub listener (one msg) for "%shown%": received message")
                                    (messageName p)

stubListenerConv
    :: (WithLogger m, Message r, Bi r)
    => Proxy r -> (ListenerSpec m, OutSpecs)
stubListenerConv p = listenerConv $ \_d __nId convActions ->
                          let _ = convActions `asProxyTypeOf` __modP p
                              __modP :: Proxy r -> Proxy (ConversationActions Void r m)
                              __modP _ = Proxy
                           in modifyLoggerName (<> "stub") $
                                logDebug $ sformat
                                    ("Stub listener (conv) for "%shown%": received message")
                                    (messageName p)

withWaitLog :: ( CanLogInParallel m ) => N.SendActions BiP PeerData m -> N.SendActions BiP PeerData m
withWaitLog sendActions = sendActions
    { N.sendTo = \nodeId msg ->
                  let MessageName mName = messageName' msg
                   in logWarningWaitLinear 4
                        (sformat ("Send "%shown%" to "%shown) mName nodeId) $
                          N.sendTo sendActions nodeId msg
    , N.withConnectionTo = \nodeId action -> N.withConnectionTo sendActions nodeId $ action . withWaitLogConv nodeId
    }

withWaitLogConv
    :: (CanLogInParallel m, Message snd)
    => N.NodeId -> N.ConversationActions PeerData snd rcv m -> N.ConversationActions PeerData snd rcv m
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
    MessageName sndMsg = messageName $ ((\_ -> Proxy) :: N.ConversationActions PeerData snd rcv m -> Proxy snd) conv

withWaitLogConvL
    :: (CanLogInParallel m, Message rcv)
    => N.NodeId -> N.ConversationActions PeerData snd rcv m -> N.ConversationActions PeerData snd rcv m
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
    MessageName rcvMsg = messageName $ ((\_ -> Proxy) :: N.ConversationActions PeerData snd rcv m -> Proxy rcv) conv

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
