{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

-- | Protocol/versioning related communication helpers.

module Pos.Communication.Protocol
       ( module Pos.Communication.Types.Protocol
       , hoistSendActions
       , mapListener
       , mapListener'
       , Message (..)
       , MessageCode
       , unpackLSpecs
       , hoistMkListeners
       , makeSendActions
       , makeEnqueueMsg
       , checkProtocolMagic
       , checkingInSpecs
       , constantListeners

       -- * OnNewSlot constraints
       , LocalOnNewSlotComm
       , OnNewSlotComm
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Buildable as B
import           Formatting (bprint, build, sformat, (%))
import           Mockable (Async, Delay, Mockable, Mockables, SharedAtomic)
import qualified Network.Broadcast.OutboundQueue as OQ
import qualified Node as N
import           Node.Message.Class (Message (..), MessageCode, messageCode)
import           Serokell.Util.Text (listJson)
import           System.Wlog (WithLogger, logWarning)

import           Pos.Communication.Types.Protocol
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Recovery.Info (MonadRecoveryInfo)
import           Pos.Reporting (MonadReporting)
import           Pos.Shutdown (HasShutdownContext)
import           Pos.Slotting (MonadSlots)

mapListener
    :: (forall t. m t -> m t) -> Listener m -> Listener m
mapListener = mapListener' $ const identity

mapListener'
    :: (forall snd rcv. Message rcv => N.NodeId
          -> N.ConversationActions snd rcv m
          -> N.ConversationActions snd rcv m)
    -> (forall t. m t -> m t) -> Listener m -> Listener m
mapListener' caMapper mapper (N.Listener f) =
    N.Listener $ \d nId -> mapper . f d nId . caMapper nId

hoistSendActions
    :: forall n m .
       ( Functor m )
    => (forall a. n a -> m a)
    -> (forall a. m a -> n a)
    -> SendActions n
    -> SendActions m
hoistSendActions nat rnat SendActions {..} = SendActions withConnectionTo' enqueueMsg''
  where
    withConnectionTo'
        :: forall t . NodeId -> (PeerData -> NonEmpty (Conversation m t)) -> m t
    withConnectionTo' nodeId k =
        nat $ withConnectionTo nodeId $ \peerData ->
            flip map (k peerData) $ \(Conversation l) ->
                Conversation $ \cactions ->
                    rnat (l (N.hoistConversationActions nat cactions))

    enqueueMsg''
        :: forall t .
           Msg
        -> (NodeId -> PeerData -> NonEmpty (Conversation m t))
        -> m (Map NodeId (m t))
    enqueueMsg'' msg k = (fmap . fmap) nat $
        nat $ enqueueMsg msg $ \peer pVI ->
            let convs = k peer pVI
                convert (Conversation l) = Conversation $ \cactions ->
                    rnat (l (N.hoistConversationActions nat cactions))
            in  map convert convs

hoistMkListeners
    :: (forall a. m a -> n a)
    -> (forall a. n a -> m a)
    -> MkListeners m
    -> MkListeners n
hoistMkListeners nat rnat (MkListeners act ins outs) = MkListeners act' ins outs
  where
    act' v p = let ls = act v p in map (N.hoistListener nat rnat) ls

makeEnqueueMsg
    :: forall m .
       ( WithLogger m
       , MonadThrow m
       )
    => VerInfo
    -> (forall t . Msg -> (NodeId -> VerInfo -> N.Conversation PackingType m t) -> m (Map NodeId (STM.TVar (OQ.PacketStatus t))))
    -> EnqueueMsg m
makeEnqueueMsg ourVerInfo enqueue = \msg mkConv -> enqueue msg $ \nodeId pVI ->
    alternativeConversations nodeId ourVerInfo pVI (mkConv nodeId pVI)

alternativeConversations
    :: forall m t .
       ( WithLogger m
       , MonadThrow m
       )
    => NodeId
    -> VerInfo -- ^ Ours
    -> VerInfo -- ^ Theirs
    -> NonEmpty (Conversation m t)
    -> N.Conversation PackingType m t
alternativeConversations nid ourVerInfo theirVerInfo convs
    | vIMagic ourVerInfo /= vIMagic theirVerInfo =
        throwErrs (one $ MismatchedProtocolMagic (vIMagic ourVerInfo) (vIMagic theirVerInfo)) (NE.head convs)
    | otherwise =
        let alts = map (checkingOutSpecs' nid (vIInHandlers theirVerInfo)) convs
        in  case sequence alts of
                Left (Conversation l) -> N.Conversation $ \conv -> do
                    mapM_ logOSNR alts
                    l conv
                Right errs -> throwErrs errs (NE.head convs)
  where

    ourOutSpecs = vIOutHandlers ourVerInfo

    throwErrs
        :: forall e x .
           ( Exception e, Buildable e )
        => NonEmpty e
        -> Conversation m x
        -> N.Conversation PackingType m x
    throwErrs errs (Conversation l) = N.Conversation $ \conv -> do
        let _ = l conv
        logWarning $ sformat
            ("Failed to choose appropriate conversation: "%listJson)
            errs
        throwM $ NE.head errs

    fstArg :: (a -> b) -> Proxy a
    fstArg _ = Proxy

    logOSNR (Right e@(OutSpecNotReported _ _)) = logWarning $ sformat build e
    logOSNR _                                  = pure ()

    checkingOutSpecs' nodeId peerInSpecs conv@(Conversation h) =
        checkingOutSpecs (sndMsgCode, ConvHandler rcvMsgCode) nodeId peerInSpecs conv
      where
        sndMsgCode = messageCode . sndProxy $ fstArg h
        rcvMsgCode = messageCode . rcvProxy $ fstArg h

    -- This is kind of last resort, in general we should handle conversation
    --    to be supported by external peer on higher level
    checkingOutSpecs spec nodeId peerInSpecs action =
        if | spec `notInSpecs` ourOutSpecs ->
                  Right $ OutSpecNotReported ourOutSpecs spec
           | spec `notInSpecs` peerInSpecs ->
                  Right $ PeerInSpecNotReported peerInSpecs nodeId spec
           | otherwise -> Left action

makeSendActions
    :: forall m .
       ( WithLogger m
       -- XXX it's MonadIO and MonadThrow... is there any sensible case in
       -- which throwM /= throwIO ?
       , MonadIO m
       , MonadThrow m
       )
    => VerInfo
    -> (forall t . Msg -> (NodeId -> VerInfo -> N.Conversation PackingType m t) -> m (Map NodeId (STM.TVar (OQ.PacketStatus t))))
    -> Converse PackingType PeerData m
    -> SendActions m
makeSendActions ourVerInfo enqueue converse = SendActions
    { withConnectionTo = \nodeId mkConv -> N.converseWith converse nodeId $ \pVI ->
          alternativeConversations nodeId ourVerInfo pVI (mkConv pVI)
      -- NB: here we don't use 'converse'; that's ultimately used by the
      -- outbound queue's dequeue thread. We just enqueue there.
    , enqueueMsg = \msg mkConv -> waitForDequeues <$> (makeEnqueueMsg ourVerInfo enqueue msg mkConv)
    } 

data SpecError
    = OutSpecNotReported HandlerSpecs (MessageCode, HandlerSpec)
    | PeerInSpecNotReported HandlerSpecs NodeId (MessageCode, HandlerSpec)
    deriving (Generic, Show)

instance Exception SpecError

instance Buildable SpecError where
    build (OutSpecNotReported outSpecs spec) =
        bprint
          ("Sending "%build%": endpoint not reported to be used for sending. Our out specs: "%build)
          spec outSpecs
    build (PeerInSpecNotReported inSpecs nodeId spec) =
        bprint
          ("Attempting to send to "%build%": endpoint unsupported by peer "%build%". In specs: "%build)
          spec nodeId inSpecs

data MismatchedProtocolMagic
    = MismatchedProtocolMagic Int32 Int32
    deriving (Generic, Show)

instance Exception MismatchedProtocolMagic

instance Buildable MismatchedProtocolMagic where
    build (MismatchedProtocolMagic ourMagic theirMagic) =
        bprint
          ("Mismatched protocolMagic, our: "%build%", their: "%build) ourMagic theirMagic


type LocalOnNewSlotComm ctx m =
    ( MonadIO m
    , MonadReader ctx m
    , MonadSlots ctx m
    , MonadMask m
    , WithLogger m
    , Mockables m [Async, Delay]
    , MonadReporting ctx m
    , HasShutdownContext ctx
    , MonadRecoveryInfo m
    , HasConfiguration
    )

type OnNewSlotComm ctx m =
    ( LocalOnNewSlotComm ctx m
    , MonadThrow m
    , Mockable SharedAtomic m
    , HasConfiguration
    )

checkProtocolMagic
    :: WithLogger m
    => VerInfo
    -> VerInfo
    -> m ()
    -> m ()
checkProtocolMagic (vIMagic -> ourMagic) (vIMagic -> theirMagic) action
    -- Check that protocolMagic is the same
    | ourMagic == theirMagic = action
    | otherwise =
        logWarning $ sformat ("Mismatched protocolMagic, our: "%build%", their: "%build) ourMagic theirMagic

checkingInSpecs
    :: WithLogger m
    => VerInfo
    -> VerInfo
    -> (MessageCode, HandlerSpec)
    -> NodeId
    -> m ()
    -> m ()
checkingInSpecs ourVerInfo peerVerInfo' spec nodeId action =
    if | spec `notInSpecs` vIInHandlers ourVerInfo ->
              logWarning $ sformat
                ("Endpoint is served, but not reported " % build) spec
       | spec `notInSpecs` vIOutHandlers peerVerInfo' ->
              logWarning $ sformat
                ("Peer " % build % " attempting to use endpoint he didn't report to use " % build)
                nodeId spec
       | otherwise -> action

rcvProxy :: Proxy (ConversationActions snd rcv m) -> Proxy rcv
rcvProxy _ = Proxy

sndProxy :: Proxy (ConversationActions snd rcv m) -> Proxy snd
sndProxy _ = Proxy

-- Provides set of listeners which doesn't depend on PeerData
constantListeners :: [(ListenerSpec m, OutSpecs)] -> MkListeners m
constantListeners = toMkL . unpackLSpecs . second mconcat . unzip
  where
    toMkL (lGet, ins, outs) = MkListeners (\vI _ -> lGet vI) ins outs

unpackLSpecs :: ([ListenerSpec m], OutSpecs) -> (VerInfo -> [Listener m], InSpecs, OutSpecs)
unpackLSpecs =
    over _1 (\ls verInfo -> fmap ($ verInfo) ls) .
    over _2 (InSpecs . HM.fromList) .
    convert . first (map lsToPair)
  where
    lsToPair (ListenerSpec h spec) = (h, spec)
    convert :: ([(l, i)], out) -> ([l], [i], out)
    convert (xs, out) = (map fst xs, map snd xs, out)
