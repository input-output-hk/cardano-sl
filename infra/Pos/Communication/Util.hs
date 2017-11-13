-- | Communication-specific utility functions.

module Pos.Communication.Util
       ( wrapListener
       , wrapActionSpec
       , wrapSendActions
       ) where

import           Universum

import           Formatting (hex, sformat, shown, (%))
import qualified Node as N
import           System.Wlog (LoggerName, modifyLoggerName)

import           Pos.Communication.Configuration (networkWaitLogInterval)
import           Pos.Communication.Protocol (ActionSpec (..), Conversation (..), Listener,
                                             Message (..), SendActions (..), mapActionSpec,
                                             mapListener)
import           Pos.Infra.Configuration (HasInfraConfiguration)
import           Pos.Util.TimeLimit (CanLogInParallel, logWarningWaitLinear)

sendActionsWithWaitLog :: ( HasInfraConfiguration, CanLogInParallel m )
            => SendActions m
            -> SendActions m
sendActionsWithWaitLog sendActions = sendActions
    { withConnectionTo = \nodeId mkConvs ->
          withConnectionTo sendActions nodeId $ \peerData ->
              fmap (introduceLog nodeId) (mkConvs peerData)
    }
  where
    introduceLog nodeId (Conversation l) = Conversation $ \cA ->
        l $ convWithWaitLog nodeId cA

convWithWaitLog
    :: (HasInfraConfiguration, CanLogInParallel m, Message snd)
    => N.NodeId
    -> N.ConversationActions snd rcv m
    -> N.ConversationActions snd rcv m
convWithWaitLog nodeId conv = conv { N.send = send', N.recv = recv' }
  where
    send' msg =
        logWarningWaitLinear networkWaitLogInterval
          (sformat ("Send "%hex%" to "%shown%" in conversation")
            sndMsg nodeId) $
           N.send conv msg
    recv' limit =
        logWarningWaitLinear networkWaitLogInterval
          (sformat ("Recv from "%shown%" in conversation") nodeId) $
           N.recv conv limit
    sndMsg = messageCode $
        ((\_ -> Proxy) :: N.ConversationActions snd rcv m -> Proxy snd) conv

-- TODO: Remove?
_convWithWaitLogL
    :: (HasInfraConfiguration, CanLogInParallel m, Message rcv)
    => N.NodeId
    -> N.ConversationActions snd rcv m
    -> N.ConversationActions snd rcv m
_convWithWaitLogL nodeId conv = conv { N.send = send', N.recv = recv' }
  where
    send' msg =
        logWarningWaitLinear networkWaitLogInterval
          (sformat ("Send to "%shown%" in conversation") nodeId) $
            N.send conv msg
    recv' limit =
        logWarningWaitLinear networkWaitLogInterval
          (sformat ("Recv "%hex%" from "%shown%" in conversation") rcvMsg nodeId) $
            N.recv conv limit
    rcvMsg = messageCode $
        ((\_ -> Proxy) :: N.ConversationActions snd rcv m -> Proxy rcv) conv

wrapListener
  :: ( CanLogInParallel m )
  => LoggerName -> Listener m -> Listener m
wrapListener lname = modifyLogger lname
  where
    modifyLogger _name = mapListener $ modifyLoggerName (<> lname)

wrapActionSpec
  :: ( CanLogInParallel m )
  => LoggerName -> ActionSpec m a -> ActionSpec m a
wrapActionSpec lname = modifyLogger lname
  where
    modifyLogger _name = mapActionSpec identity $ modifyLoggerName
                                    (<> lname)

wrapSendActions
  :: ( HasInfraConfiguration, CanLogInParallel m )
  => SendActions m
  -> SendActions m
wrapSendActions =
    sendActionsWithWaitLog
