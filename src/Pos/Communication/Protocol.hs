{-# LANGUAGE RankNTypes #-}

-- | Protocol/versioning related communication helpers.

module Pos.Communication.Protocol
       ( module Pos.Communication.Types.Protocol
       , listenerOneMsg
       , listenerConv
       , hoistSendActions
       , mapListener
       , mapListener'
       , Message (..)
       , MessageName (..)
       , messageName'
       , worker
       , toAction
       ) where

import qualified Data.HashMap.Strict              as HM
import           Data.Proxy                       (Proxy (..))
import qualified Data.Text.Buildable              as B
import           Formatting                       (build, sformat, shown, stext, (%))
import           Mockable                         (Mockable, Throw, throw)
import qualified Node                             as N
import           Node.Message                     (Message (..), MessageName (..),
                                                   messageName')
import           Serokell.Util.Base16             (base16F)
import           System.Wlog                      (WithLogger, logDebug, logWarning)
import           Universum

import           Pos.Binary.Class                 (Bi)
import           Pos.Communication.BiP            (BiP)
import           Pos.Communication.Types.Protocol
mapListener
    :: (forall t. m t -> m t) -> Listener m -> Listener m
mapListener = mapListener' identity $ const identity

mapListener'
    :: (N.SendActions BiP PeerId m -> N.SendActions BiP PeerId m)
    -> (forall snd rcv. Message rcv => N.NodeId
          -> N.ConversationActions PeerId snd rcv m
          -> N.ConversationActions PeerId snd rcv m)
    -> (forall t. m t -> m t) -> Listener m -> Listener m
mapListener' saMapper _ mapper (N.ListenerActionOneMsg f) =
    N.ListenerActionOneMsg $ \d nId sA -> mapper . f d nId (saMapper sA)
mapListener' _ caMapper mapper (N.ListenerActionConversation f) =
    N.ListenerActionConversation $ \d nId -> mapper . f d nId . caMapper nId

hoistConversationActions
    :: (forall a. n a -> m a)
    -> ConversationActions body rcv n
    -> ConversationActions body rcv m
hoistConversationActions nat ConversationActions {..} =
    ConversationActions send' recv'
  where
    send' = nat . send
    recv' = nat recv

hoistSendActions
    :: (forall a. n a -> m a)
    -> (forall a. m a -> n a)
    -> SendActions n
    -> SendActions m
hoistSendActions nat rnat SendActions {..} = SendActions sendTo' withConnectionTo'
  where
    sendTo' nodeId msg = nat $ sendTo nodeId msg
    withConnectionTo' nodeId convActionsH =
        nat $ withConnectionTo nodeId $ \convActions -> rnat $ convActionsH $ hoistConversationActions nat convActions

convertCA :: N.ConversationActions PeerId snd rcv m -> ConversationActions snd rcv m
convertCA cA = ConversationActions
    { send = N.send cA
    , recv = N.recv cA
    }

convertSA :: N.SendActions BiP PeerId m -> SendActions m
convertSA sA = SendActions
    { sendTo = \(NodeId (peerId, nNodeId)) -> N.sendTo sA nNodeId
    , withConnectionTo = \(NodeId (peerId, nNodeId)) h ->
                              N.withConnectionTo sA nNodeId $ h . convertCA
    }

listenerOneMsg :: ( Bi msg, Message msg )
    => (VerInfo -> NodeId -> SendActions m -> msg -> m ())
    -> N.ListenerAction BiP PeerId m
listenerOneMsg h =
    N.ListenerActionOneMsg $ \peerId nNodeId sA ->
        h undefined (NodeId (peerId, nNodeId)) (convertSA sA)

listenerConv :: ( Bi snd, Bi rcv, Message snd, Message rcv )
    => (VerInfo -> NodeId -> ConversationActions snd rcv m -> m ())
    -> N.ListenerAction BiP PeerId m
listenerConv h =
    N.ListenerActionConversation $ \peerId nNodeId conv -> h undefined (NodeId (peerId, nNodeId)) (convertCA conv)

toAction :: (SendActions m -> m a) -> Action m a
toAction h = h . convertSA

worker :: (SendActions m -> m ()) -> Worker m
worker = toAction

--worker :: (WithLogger m)
--    => (SendActions m -> m ())
--    -> WorkerSpecs m
--worker specs run = WorkerSpecs [const $ toAction run] mempty

-- listenerConv :: (WithLogger m, Bi snd, Bi rcv, Message snd, Message rcv)
--     => (NodeId -> ConversationActions PeerId snd rcv m -> m ())
--     -> (PeerId -> Listener m, (MessageName, HandlerSpec))
-- listenerConv handler = (listener, spec)
--   where
--     spec = (rcvMsgName, ConvHandler sndMsgName)
--     convProxy = convProxy' handler
--     convProxy' :: (a -> b -> c) -> Proxy b
--     convProxy' _ = Proxy
--     sndMsgName = messageName $ sndProxy convProxy
--     rcvMsgName = messageName $ rcvProxy convProxy
--     -- TODO specs parameter is to be received within listener
--     listener ourPeerId =
--       ListenerActionConversation $ \peerPeerId peerId conv ->
--           checkingInSpecs ourPeerId peerPeerId spec peerId $
--               handler peerId conv
--
-- listenerOneMsg :: (WithLogger m, Bi msg, Message msg, Mockable Throw m)
--     => (NodeId -> SendActions m -> msg -> m ())
--     -> (VersionInfo -> Listener m, (MessageName, HandlerSpec))
-- listenerOneMsg handler = (listener, spec)
--   where
--     spec = (rcvMsgName, OneMsgHandler)
--     msgProxy :: (a -> b -> msg -> c) -> Proxy msg
--     msgProxy _ = Proxy
--     rcvMsgName = messageName $ msgProxy handler
--     listener ourPeerId =
--       ListenerActionOneMsg $ \peerPeerId peerId sA msg ->
--           checkingInSpecs ourPeerId peerPeerId spec peerId $
--               handler peerId (modifySend (vIOutHandlers ourPeerId) sA) msg
--
-- checkingInSpecs :: WithLogger m => VersionInfo -> VersionInfo -> (MessageName, HandlerSpec) -> PeerId -> m () -> m ()
-- checkingInSpecs ourPeerId peerPeerId spec peerId action =
--     if | spec `notInSpecs` vIInHandlers ourPeerId ->
--               logWarning $ sformat
--                 ("Endpoint is served, but not reported " % build) spec
--        | spec `notInSpecs` vIOutHandlers peerPeerId ->
--               logDebug $ sformat
--                 ("Peer " % shown % " attempting to use endpoint he didn't report to use " % build)
--                 peerId spec
--        | otherwise -> action
--
-- rcvProxy :: Proxy (ConversationActions d snd rcv m) -> Proxy rcv
-- rcvProxy _ = Proxy
-- sndProxy :: Proxy (ConversationActions d snd rcv m) -> Proxy snd
-- sndProxy _ = Proxy
--
-- data SpecError = OutSpecNotReported MessageName
--                | PeerInSpecNotReported MessageName
--   deriving (Generic, Show)
--
-- instance Exception SpecError
--
-- modifySend :: (WithLogger m, Mockable Throw m)
--            => HandlerSpecs -> SendActions m -> SendActions m
-- modifySend ourOutSpecs sA = sA
--     { sendTo = \nodeId msg ->
--           let sndMsgName = messageName' msg
--            in checkingSpecs (Left sndMsgName) peerId $
--                   sendTo sA nodeId msg
--     , withConnectionTo = \nodeId convAction ->
--           let sndMsgName = messageName . sndProxy $ fstArgProxy convAction
--            in checkingSpecs (Right sndMsgName) peerId $
--                   withConnectionTo sA nodeId convAction
--     }
--   where
--     -- TODO update code
--     peerInSpecs = ourOutSpecs
--
--     fstArgProxy :: (a -> b) -> Proxy a
--     fstArgProxy _ = Proxy
--
--     notInSpecs' (Left name) specs = (name, OneMsgHandler) `notInSpecs` specs
--     notInSpecs' (Right name) specs = case name `HM.lookup` specs of
--                                         Just (ConvHandler _) -> True
--                                         _                    -> False
--     checkingSpecs spec peerId action =
--         if | spec `notInSpecs'` ourOutSpecs -> do
--                   logWarning $ sformat
--                      ("Sending "%stext%": endpoint not reported")
--                      (fS spec)
--                   throw' OutSpecNotReported spec
--            | spec `notInSpecs'` peerInSpecs -> do
--                   logDebug $ sformat
--                      ("Attempting to send to "%stext%": endpoint unsupported by peer "%shown)
--                      (fS spec) peerId
--                   throw' PeerInSpecNotReported spec
--            | otherwise -> action
--       where
--         throw' constr = throw . constr . either identity identity
--         fS (Left m)                = sformat build (m, OneMsgHandler)
--         fS (Right (MessageName m)) = sformat ("("%base16F%", Conv _)") m
