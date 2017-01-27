{-# LANGUAGE RankNTypes #-}

-- | Protocol/versioning related communication helpers.

module Pos.Communication.Protocol
       ( listenerConv
       , listenerOneMsg
       --, worker
       ) where

import qualified Data.HashMap.Strict              as HM
import           Data.Proxy                       (Proxy (..))
import           Formatting                       (build, sformat, shown, stext, (%))
import           Mockable                         (Mockable, Throw, throw)
import           Node                             (ConversationActions (..), Listener,
                                                   ListenerAction (..), NodeId,
                                                   SendActions (..), Worker)
import           Node.Message                     (Message (..), MessageName (..),
                                                   messageName')
import           Serokell.Util.Base16             (base16F)
import           System.Wlog                      (WithLogger, logDebug, logWarning)
import           Universum

import           Pos.Binary.Class                 (Bi)
import           Pos.Communication.BiP            (BiP)
import           Pos.Communication.Types.Protocol (HandlerSpec (..), HandlerSpecs,
                                                   VerInfo (..), notInSpecs)


--worker :: (WithLogger m)
--    => Worker BiP m
--    -> HandlerSpecs
--    -> (VerInfo -> Worker BiP m,

listenerConv :: (WithLogger m, Bi snd, Bi rcv, Message snd, Message rcv)
    => (NodeId -> ConversationActions snd rcv m -> m ())
    -> (VerInfo -> VerInfo -> Listener BiP m, (MessageName, HandlerSpec))
listenerConv handler = (listener, spec)
  where
    spec = (rcvMsgName, ConvHandler sndMsgName)
    convProxy = convProxy' handler
    convProxy' :: (NodeId -> ConversationActions snd rcv m -> m ()) -> Proxy (ConversationActions snd rcv m)
    convProxy' _ = Proxy
    sndMsgName = messageName $ sndProxy convProxy
    rcvMsgName = messageName $ rcvProxy convProxy
    -- TODO specs parameter is to be received within listener
    listener ourVerInfo peerVerInfo =
      ListenerActionConversation $ \peerId conv ->
          checkingInSpecs ourVerInfo peerVerInfo spec peerId $
              handler peerId conv

listenerOneMsg :: (WithLogger m, Bi msg, Message msg, Mockable Throw m)
    => (NodeId -> SendActions BiP m -> msg -> m ())
    -> (VerInfo -> VerInfo -> Listener BiP m, (MessageName, HandlerSpec))
listenerOneMsg handler = (listener, spec)
  where
    spec = (rcvMsgName, OneMsgHandler)
    msgProxy :: (NodeId -> SendActions BiP m -> msg -> m ()) -> Proxy msg
    msgProxy _ = Proxy
    rcvMsgName = messageName $ msgProxy handler
    listener ourVerInfo peerVerInfo =
      ListenerActionOneMsg $ \peerId sA msg ->
          checkingInSpecs ourVerInfo peerVerInfo spec peerId $
              handler peerId (modifySend (vIOutHandlers ourVerInfo) sA) msg

checkingInSpecs :: WithLogger m => VerInfo -> VerInfo -> (MessageName, HandlerSpec) -> NodeId -> m () -> m ()
checkingInSpecs ourVerInfo peerVerInfo spec peerId action =
    if | spec `notInSpecs` vIInHandlers ourVerInfo ->
              logWarning $ sformat
                ("Endpoint is served, but not reported " % build) spec
       | spec `notInSpecs` vIOutHandlers peerVerInfo ->
              logDebug $ sformat
                ("Peer " % shown % " attempting to use endpoint he didn't report to use " % build)
                peerId spec
       | otherwise -> action

rcvProxy :: Proxy (ConversationActions snd rcv m) -> Proxy rcv
rcvProxy _ = Proxy
sndProxy :: Proxy (ConversationActions snd rcv m) -> Proxy snd
sndProxy _ = Proxy

data SpecError = OutSpecNotReported MessageName
               | PeerInSpecNotReported MessageName
  deriving (Generic, Show)

instance Exception SpecError

modifySend :: (WithLogger m, Mockable Throw m)
           => HandlerSpecs -> SendActions BiP m -> SendActions BiP m
modifySend ourOutSpecs sA = sA
    { sendTo = \peerId msg ->
          let sndMsgName = messageName' msg
           in checkingSpecs (Left sndMsgName) peerId $
                  sendTo sA peerId msg
    , withConnectionTo = \peerId convAction ->
          let sndMsgName = messageName . sndProxy $ fstArgProxy convAction
           in checkingSpecs (Right sndMsgName) peerId $
                  withConnectionTo sA peerId convAction
    }
  where
    -- TODO update code
    peerInSpecs = ourOutSpecs

    fstArgProxy :: (a -> b) -> Proxy a
    fstArgProxy _ = Proxy

    notInSpecs' (Left name) specs = (name, OneMsgHandler) `notInSpecs` specs
    notInSpecs' (Right name) specs = case name `HM.lookup` specs of
                                        Just (ConvHandler _) -> True
                                        _                    -> False
    checkingSpecs spec peerId action =
        if | spec `notInSpecs'` ourOutSpecs -> do
                  logWarning $ sformat
                     ("Sending "%stext%": endpoint not reported")
                     (fS spec)
                  throw' OutSpecNotReported spec
           | spec `notInSpecs'` peerInSpecs -> do
                  logDebug $ sformat
                     ("Attempting to send to "%stext%": endpoint unsupported by peer "%shown)
                     (fS spec) peerId
                  throw' PeerInSpecNotReported spec
           | otherwise -> action
      where
        throw' constr = throw . constr . either identity identity
        fS (Left m)                = sformat build (m, OneMsgHandler)
        fS (Right (MessageName m)) = sformat ("("%base16F%", Conv _)") m
