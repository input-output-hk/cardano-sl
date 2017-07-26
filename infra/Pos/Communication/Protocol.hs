{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Protocol/versioning related communication helpers.

module Pos.Communication.Protocol
       ( module Pos.Communication.Types.Protocol
       , hoistSendActions
       , mapListener
       , mapListener'
       , mapActionSpec
       , Message (..)
       , MessageName (..)
       , messageName'
       , worker
       , worker'
       , localWorker
       , localSpecs
       , toAction
       , unpackLSpecs
       , hoistMkListeners
       , onNewSlotWorker
       , localOnNewSlotWorker
       , onNewSlotWithLoggingWorker
       , makeSendActions
       , makeEnqueueMsg
       , checkingInSpecs
       , constantListeners
       ) where

import qualified Data.HashMap.Strict              as HM
import qualified Data.List.NonEmpty               as NE
import qualified Data.Text.Buildable              as B
import           Formatting                       (bprint, build, sformat, (%))
import           Mockable                         (Delay, Fork, Mockable, Mockables,
                                                   SharedAtomic, Throw, throw)
import qualified Node                             as N
import           Node.Message.Class               (Message (..), MessageName (..),
                                                   messageName')
import           Serokell.Util.Text               (listJson)
import           System.Wlog                      (WithLogger, logWarning)
import           Universum

import           Pos.Communication.Types.Protocol
import           Pos.Core.Types                   (SlotId)
import           Pos.KnownPeers                   (MonadKnownPeers)
import           Pos.Recovery.Info                (MonadRecoveryInfo)
import           Pos.Reporting                    (HasReportingContext)
import           Pos.Shutdown                     (HasShutdownContext)
import           Pos.Slotting                     (MonadSlots)
import           Pos.Slotting.Util                (onNewSlot, onNewSlotImpl)

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

mapActionSpec
    :: (SendActions m -> SendActions m)
    -> (forall t. m t -> m t) -> ActionSpec m a -> ActionSpec m a
mapActionSpec saMapper aMapper (ActionSpec f) =
    ActionSpec $ \vI sA -> aMapper $ f vI (saMapper sA)

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
    :: ( Monad n, Functor m )
    => (forall a. m a -> n a)
    -> (forall a. n a -> m a)
    -> MkListeners m
    -> MkListeners n
hoistMkListeners nat rnat (MkListeners act ins outs) = MkListeners act' ins outs
  where
    act' v p = let ls = act v p in map (N.hoistListener nat rnat) ls

makeEnqueueMsg
    :: forall m .
       ( WithLogger m
       , Mockable Throw m
       , Mockable SharedAtomic m
       )
    => VerInfo
    -> (forall t . Msg -> (NodeId -> VerInfo -> N.Conversation PackingType m t) -> m (Map NodeId (m t)))
    -> EnqueueMsg m
makeEnqueueMsg ourVerInfo enqueue = \msg mkConv -> enqueue msg $ \nodeId pVI ->
    alternativeConversations nodeId ourVerInfo pVI (mkConv nodeId pVI)

alternativeConversations
    :: forall m t .
       ( WithLogger m
       , Mockable Throw m
       )
    => NodeId
    -> VerInfo -- ^ Ours
    -> VerInfo -- ^ Theirs
    -> NonEmpty (Conversation m t)
    -> N.Conversation PackingType m t
alternativeConversations nid ourVerInfo theirVerInfo convs =
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
        throw $ NE.head errs

    fstArg :: (a -> b) -> Proxy a
    fstArg _ = Proxy

    logOSNR (Right e@(OutSpecNotReported _)) = logWarning $ sformat build e
    logOSNR _                                = pure ()

    checkingOutSpecs' nodeId peerInSpecs conv@(Conversation h) =
        checkingOutSpecs (sndMsgName, ConvHandler rcvMsgName) nodeId peerInSpecs conv
      where
        sndMsgName = messageName . sndProxy $ fstArg h
        rcvMsgName = messageName . rcvProxy $ fstArg h

    -- This is kind of last resort, in general we should handle conversation
    --    to be supported by external peer on higher level
    checkingOutSpecs spec nodeId peerInSpecs action =
        if | spec `notInSpecs` ourOutSpecs ->
                  Right $ OutSpecNotReported spec
           | spec `notInSpecs` peerInSpecs ->
                  Right $ PeerInSpecNotReported nodeId spec
           | otherwise -> Left action

makeSendActions
    :: forall m .
       ( WithLogger m
       , Mockable Throw m
       , Mockable SharedAtomic m
       )
    => VerInfo
    -> (forall t . Msg -> (NodeId -> VerInfo -> N.Conversation PackingType m t) -> m (Map NodeId (m t)))
    -> Converse PackingType PeerData m
    -> SendActions m
makeSendActions ourVerInfo enqueue converse = SendActions
    { withConnectionTo = \nodeId mkConv -> N.converseWith converse nodeId $ \pVI ->
          alternativeConversations nodeId ourVerInfo pVI (mkConv pVI)
    , enqueueMsg = makeEnqueueMsg ourVerInfo enqueue
    }

data SpecError
    = OutSpecNotReported (MessageName, HandlerSpec)
    | PeerInSpecNotReported NodeId (MessageName, HandlerSpec)
    deriving (Generic, Show)

instance Exception SpecError

instance Buildable SpecError where
    build (OutSpecNotReported spec) =
        bprint
          ("Sending "%build%": endpoint not reported to be used for sending")
          spec
    build (PeerInSpecNotReported nodeId spec) =
        bprint
          ("Attempting to send to "%build%": endpoint unsupported by peer "%build)
          spec nodeId

type WorkerConstr m =
    ( WithLogger m
    , Mockable Throw m
    , Mockable SharedAtomic m
    )

toAction
    :: WorkerConstr m
    => (SendActions m -> m a) -> ActionSpec m a
toAction h = ActionSpec $ const h

worker
    :: WorkerConstr m
    => OutSpecs -> Worker m -> (WorkerSpec m, OutSpecs)
worker outSpecs = (,outSpecs) . toAction

workerHelper
    :: WorkerConstr m
    => OutSpecs -> (arg -> Worker m) -> (arg -> WorkerSpec m, OutSpecs)
workerHelper outSpecs h = (,outSpecs) $ toAction . h

worker'
    :: WorkerConstr m
    => OutSpecs -> (VerInfo -> Worker m) -> (WorkerSpec m, OutSpecs)
worker' outSpecs h =
    (,outSpecs) $ ActionSpec $ h


type LocalOnNewSlotComm ctx m =
    ( MonadIO m
    , MonadReader ctx m
    , MonadSlots m
    , MonadMask m
    , WithLogger m
    , Mockables m [Fork, Delay]
    , HasReportingContext ctx
    , HasShutdownContext ctx
    , MonadRecoveryInfo m
    , MonadKnownPeers m
    )

type OnNewSlotComm ctx m =
    ( LocalOnNewSlotComm ctx m
    , Mockable Throw m
    , Mockable SharedAtomic m
    )

onNewSlot'
    :: OnNewSlotComm ctx m
    => Bool -> Bool -> (SlotId -> WorkerSpec m, outSpecs) -> (WorkerSpec m, outSpecs)
onNewSlot' withLog startImmediately (h, outs) =
    (,outs) . ActionSpec $ \vI sA ->
        onNewSlotImpl withLog startImmediately $
            \slotId -> let ActionSpec h' = h slotId
                        in h' vI sA
onNewSlotWorker
    :: OnNewSlotComm ctx m
    => Bool -> OutSpecs -> (SlotId -> Worker m) -> (WorkerSpec m, OutSpecs)
onNewSlotWorker b outs = onNewSlot' False b . workerHelper outs

onNewSlotWithLoggingWorker
    :: OnNewSlotComm ctx m
    => Bool -> OutSpecs -> (SlotId -> Worker m) -> (WorkerSpec m, OutSpecs)
onNewSlotWithLoggingWorker b outs = onNewSlot' True b . workerHelper outs

localOnNewSlotWorker
    :: LocalOnNewSlotComm ctx m
    => Bool -> (SlotId -> m ()) -> (WorkerSpec m, OutSpecs)
localOnNewSlotWorker b h = (ActionSpec $ \__vI __sA -> onNewSlot b h, mempty)

localWorker :: m () -> (WorkerSpec m, OutSpecs)
localWorker = localSpecs

localSpecs :: m a -> (ActionSpec m a, OutSpecs)
localSpecs h = (ActionSpec $ \__vI __sA -> h, mempty)

checkingInSpecs
    :: WithLogger m
    => VerInfo
    -> VerInfo
    -> (MessageName, HandlerSpec)
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
constantListeners :: Monad m => [(ListenerSpec m, OutSpecs)] -> MkListeners m
constantListeners = toMkL . unpackLSpecs . second mconcat . unzip
  where
    toMkL (lGet, ins, outs) = MkListeners (\vI _ -> lGet vI) ins outs

unpackLSpecs :: Monad m => ([ListenerSpec m], OutSpecs) -> (VerInfo -> [Listener m], InSpecs, OutSpecs)
unpackLSpecs =
    over _1 (\ls verInfo -> fmap ($ verInfo) ls) .
    over _2 (InSpecs . HM.fromList) .
    convert . first (map lsToPair)
  where
    lsToPair (ListenerSpec h spec) = (h, spec)
    convert :: Monoid out => ([(l, i)], out) -> ([l], [i], out)
    convert (xs, out) = (map fst xs, map snd xs, out)
