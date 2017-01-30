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
       , worker'
       , localWorker
       , localWorker'
       , toAction
       , unpackLSpecs
       , hoistListenerSpec
       , onNewSlotWorker
       , localOnNewSlotWorker
       , onNewSlotWithLoggingWorker
       , convertSendActions
       ) where

import           Control.Arrow                    ((&&&))
import qualified Data.HashMap.Strict              as HM
import           Mockable                         (Delay, Fork, Mockable)
import qualified Node                             as N
import           Node.Message                     (Message (..), MessageName (..),
                                                   messageName')
import           System.Wlog                      (WithLogger)
import           Universum

import           Pos.Binary.Class                 (Bi)
import           Pos.Communication.BiP            (BiP)
import           Pos.Communication.Types.Protocol
import           Pos.Slotting                     (MonadSlots, onNewSlot',
                                                   onNewSlotWithLogging')
import           Pos.Types                        (SlotId)

mapListener
    :: (forall t. m t -> m t) -> Listener m -> Listener m
mapListener = mapListener' identity $ const identity

mapListener'
    :: (N.SendActions BiP PeerData m -> N.SendActions BiP PeerData m)
    -> (forall snd rcv. Message rcv => N.NodeId
          -> N.ConversationActions PeerData snd rcv m
          -> N.ConversationActions PeerData snd rcv m)
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

hoistListenerSpec :: (forall a. m a -> n a) -> (forall a. n a -> m a) -> ListenerSpec m -> ListenerSpec n
hoistListenerSpec nat rnat (ListenerSpec h s) =
    ListenerSpec (\vI -> N.hoistListenerAction nat rnat $ h vI) s

convertCA :: N.ConversationActions PeerData snd rcv m -> ConversationActions snd rcv m
convertCA cA = ConversationActions
    { send = N.send cA
    , recv = N.recv cA
    }

convertSendActions :: N.SendActions BiP PeerData m -> SendActions m
convertSendActions sA = SendActions
    { sendTo = \(NodeId (peerId, nNodeId)) -> N.sendTo sA nNodeId
    , withConnectionTo = \(NodeId (peerId, nNodeId)) h ->
                              N.withConnectionTo sA nNodeId $ h . convertCA
    }

listenerOneMsg :: (Bi msg, Message msg)
    => OutSpecs
    -> (VerInfo -> NodeId -> SendActions m -> msg -> m ())
    -> (ListenerSpec m, OutSpecs)
listenerOneMsg outSpecs h = (spec, outSpecs)
  where
    spec = ListenerSpec
              (\ourVerInfo -> N.ListenerActionOneMsg $
                  \(peerId, __peerData) nNodeId sA ->
                      h ourVerInfo
                        (NodeId (peerId, nNodeId))
                        (convertSendActions sA))
              (undefined, undefined)

listenerConv :: (Bi snd, Bi rcv, Message snd, Message rcv)
    => (VerInfo -> NodeId -> ConversationActions snd rcv m -> m ())
    -> (ListenerSpec m, OutSpecs)
listenerConv h = (spec, mempty)
  where
    spec = ListenerSpec
              (\ourVerInfo -> N.ListenerActionConversation $
                  \(peerId, __peerData) nNodeId conv ->
                      h ourVerInfo
                        (NodeId (peerId, nNodeId))
                        (convertCA conv))
              (undefined, undefined)

unpackLSpecs :: ([ListenerSpec m], OutSpecs) -> (VerInfo -> [Listener m], InSpecs, OutSpecs)
unpackLSpecs =
    over _1 (\ls verInfo -> map ($ verInfo) ls) .
    over _2 (InSpecs . HM.fromList) .
    convert . first (map lsToPair)
  where
    lsToPair (ListenerSpec h spec) = (h, spec)
    convert :: Monoid out => ([(l, i)], out) -> ([l], [i], out)
    convert = uncurry (uncurry (,,))
                . first squashPL
    squashPL :: [(a, b)] -> ([a], [b])
    squashPL = map fst &&& map snd


toAction :: (SendActions m -> m a) -> Action m a
toAction h = h . convertSendActions

worker :: OutSpecs -> Worker' m -> (Worker m, OutSpecs)
worker outSpecs = (,outSpecs) . toAction

worker' :: OutSpecs -> (arg -> Worker' m) -> (arg -> Worker m, OutSpecs)
worker' outSpecs h = (,outSpecs) $ toAction . h

onNewSlotWorker
    :: ( MonadIO m
       , MonadSlots m
       , MonadCatch m
       , WithLogger m
       , Mockable Fork m
       , Mockable Delay m
       ) => Bool -> OutSpecs -> (SlotId -> Worker' m) -> (Worker m, OutSpecs)
onNewSlotWorker b outs = onNewSlot' b . worker' outs

onNewSlotWithLoggingWorker
    :: ( MonadIO m
       , MonadSlots m
       , MonadCatch m
       , WithLogger m
       , Mockable Fork m
       , Mockable Delay m
       ) => Bool -> OutSpecs -> (SlotId -> Worker' m) -> (Worker m, OutSpecs)
onNewSlotWithLoggingWorker b outs = onNewSlotWithLogging' b . worker' outs

localOnNewSlotWorker
    :: ( MonadIO m
       , MonadSlots m
       , MonadCatch m
       , WithLogger m
       , Mockable Fork m
       , Mockable Delay m
       ) =>  Bool -> (SlotId -> m ()) -> (Worker m, OutSpecs)
localOnNewSlotWorker b h = onNewSlotWorker b mempty $ const . h

localWorker :: m () -> (Worker m, OutSpecs)
localWorker = worker mempty . const

localWorker' :: (arg -> m ()) -> (arg -> Worker m, OutSpecs)
localWorker' h = worker' mempty $ const . h

--worker :: (WithLogger m)
--    => (SendActions m -> m ())
--    -> WorkerSpecs m
--worker specs run = WorkerSpecs [const $ toAction run] mempty

-- listenerConv :: (WithLogger m, Bi snd, Bi rcv, Message snd, Message rcv)
--     => (NodeId -> ConversationActions PeerData snd rcv m -> m ())
--     -> (VerInfo -> Listener m, (MessageName, HandlerSpec))
-- listenerConv handler = (listener, spec)
--   where
--     spec = (rcvMsgName, ConvHandler sndMsgName)
--     convProxy = convProxy' handler
--     convProxy' :: (a -> b -> c) -> Proxy b
--     convProxy' _ = Proxy
--     sndMsgName = messageName $ sndProxy convProxy
--     rcvMsgName = messageName $ rcvProxy convProxy
--     -- TODO specs parameter is to be received within listener
--     listener ourVerInfo =
--       ListenerActionConversation $ \peerVerInfo peerId conv ->
--           checkingInSpecs ourVerInfo peerVerInfo spec peerId $
--               handler peerId conv
--
-- listenerOneMsg :: (WithLogger m, Bi msg, Message msg, Mockable Throw m)
--     => (NodeId -> SendActions m -> msg -> m ())
--     -> (VerInfo -> Listener m, (MessageName, HandlerSpec))
-- listenerOneMsg handler = (listener, spec)
--   where
--     spec = (rcvMsgName, OneMsgHandler)
--     msgProxy :: (a -> b -> msg -> c) -> Proxy msg
--     msgProxy _ = Proxy
--     rcvMsgName = messageName $ msgProxy handler
--     listener ourVerInfo =
--       ListenerActionOneMsg $ \peerVerInfo peerId sA msg ->
--           checkingInSpecs ourVerInfo peerVerInfo spec peerId $
--               handler peerId (modifySend (vIOutHandlers ourVerInfo) sA) msg
--
-- checkingInSpecs :: WithLogger m => VerInfo -> VerInfo -> (MessageName, HandlerSpec) -> PeerId -> m () -> m ()
-- checkingInSpecs ourVerInfo peerVerInfo spec peerId action =
--     if | spec `notInSpecs` vIInHandlers ourVerInfo ->
--               logWarning $ sformat
--                 ("Endpoint is served, but not reported " % build) spec
--        | spec `notInSpecs` vIOutHandlers peerVerInfo ->
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
