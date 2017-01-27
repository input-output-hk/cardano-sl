{-# LANGUAGE RankNTypes #-}

-- | Protocol/versioning related communication helpers.

module Pos.Communication.Protocol
       (
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
import           Pos.Communication.Types.Protocol (HandlerSpec (..), HandlerSpecs, PeerId,
                                                   VerInfo (..), WorkerSpecs (..),
                                                   notInSpecs)
--worker' :: (WithLogger m)
--    => HandlerSpecs
--    -> (PeerId -> Worker BiP PeerId m)
--    -> WorkerSpecs PeerId m
--worker' specs run = WorkerSpecs [run'] specs
--  where
--    run' ourPeerId sA = run ourPeerId (

-- listenerConv :: (WithLogger m, Bi snd, Bi rcv, Message snd, Message rcv)
--     => (NodeId -> ConversationActions PeerId snd rcv m -> m ())
--     -> (PeerId -> Listener BiP PeerId m, (MessageName, HandlerSpec))
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
--     => (NodeId -> SendActions BiP PeerId m -> msg -> m ())
--     -> (VersionInfo -> Listener BiP PeerId m, (MessageName, HandlerSpec))
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
--            => HandlerSpecs -> SendActions BiP PeerId m -> SendActions BiP PeerId m
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
