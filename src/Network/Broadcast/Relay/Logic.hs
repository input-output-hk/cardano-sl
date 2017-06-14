-- | Framework for Inv\/Req\/Data message handling

{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}

module Network.Broadcast.Relay.Logic
    ( relayListeners
    , simpleRelayer

      -- | Listeners for Inv, Req, and Data
    , handleInvL
    , handleReqL
    , handleDataL
    ) where

import           Universum
import           Formatting                         (build, sformat, (%))
import           System.Wlog                        (WithLogger, logDebug, logWarning)

import           Mockable                           (Mockable, Throw)
import qualified Mockable.Concurrent                as Concurrent
import qualified Mockable.Channel                   as Channel
import           Node                               (Listener, ListenerAction (..),
                                                     SendActions (..), Conversation (..),
                                                     ConversationActions (..),
                                                     NodeId)
import qualified Node.Message.Class                 as Msg
import           Network.Broadcast.Relay.Types
import           Network.Broadcast.Relay.Class
import           Network.Broadcast.Relay.Util

-- [Note: relaying induced by a listener]
--
-- This is the typical relay case: rather than than initiating the relay by
-- sending 'InvMsg' to a bunch of peers, a node may be an intermediate step
-- in the relaying, responding to an 'InvMsg' by sending a 'ReqMsg', receiving
-- a 'DataMsg', and then sending an 'InvMsg' to a bunch of its peers.
--
-- This negotiation takes place inside a listener. Naturally we might want to
-- do the relay from within that listener, but we can't because a listener does
-- not have access to a node's 'SendActions'. This is for good reason: once
-- the 'DataMsg' has come and its contents processed, the listener is finished.
-- The initiating peer should not hold open its connection, nor should the
-- contacted peer.
--
-- This is why a relay propagation queue was introduced in the original relay
-- system, and that's why it remains in this one. A listener which wishes to
-- relay will simply dump the `PropagationMsg` into the queue, and some other
-- system takes care of clearing that queue, possibly according to some
-- sohpisticated QoS policy.

-- | Create listeners for one particular relay description. Include these
--   as listeners in your node and it will carry out a relay broadcast for
--   this particular relay description.
listenersForRelay
    :: forall packingType peerData m .
       ( Msg.Message Void
       , Msg.Serializable packingType Void
       , WithLogger m
       , Mockable Throw m
       )
    => Relay packingType m
    -> [Listener packingType peerData m]
listenersForRelay (InvReqData mP irdP@InvReqDataParams{..}) =
    [handleReqL handleReq, handleInvL mP irdP]
listenersForRelay (Data mP DataParams{..}) =
    [handleDataL mP handleDataOnly]

-- | Create listeners for a list of relay descriptions. Include these as
--   listeners in your node and it will carry out a relay broadcast.
relayListeners
    :: forall packingType peerData m .
       ( Msg.Message Void
       , Msg.Serializable packingType Void
       , WithLogger m
       , Mockable Throw m
       )
    => [Relay packingType m]
    -> [Listener packingType peerData m]
relayListeners = (>>= listenersForRelay)

-- | Create a relayed which uses a single bounded queue for every message to
--   be relayed. The first component is the relayed to be included in the
--   Relay descriptor definitions (for particular data types) and the second
--   component will, given network capabilities, forever pull from the queue and
--   relay the messages (you should probably spawn a thread for it).
--
--   TODO use a bounded queue!
--
--   TODO currently we take an 'Maybe NodeId -> m (Set NodeId)' to discover who
--   we ought to relay to. The parameter is the node who relayed it to us, or
--   Nothing if we're initiating. We may want to replace this with the
--   Discovery abstraction.
simpleRelayer
    :: forall packingType peerData m .
       ( Msg.Serializable packingType Void
       , Mockable Channel.Channel m
       , Mockable Concurrent.Concurrently m
       , WithLogger m
       )
    => (Maybe NodeId -> m (Set NodeId))
    -> m (PropagationMsg packingType -> m (), SendActions packingType peerData m -> m ())
simpleRelayer getTargets = do
    queue <- Channel.newChannel

    let fillQueue :: PropagationMsg packingType -> m ()
        fillQueue = Channel.writeChannel queue

    let clearQueue :: SendActions packingType peerData m -> m ()
        clearQueue sactions = do
            msg <- Channel.readChannel queue
            propagateOne sactions msg
            clearQueue sactions

    return (fillQueue, clearQueue)

  where

    -- Propagate one message to a set of peers.
    -- It does so concurrently, one thread for each peer, and will not finish
    -- until all have finished.
    propagateOne :: SendActions packingType peerData m -> PropagationMsg packingType -> m ()
    propagateOne sactions (InvReqDataPM mPeer key value) = do
        logDebug $ sformat ("Propagation data with key: "%build) key
        targets <- getTargets mPeer
        void $ Concurrent.forConcurrently (toList targets) $ \peer ->
            withConnectionTo sactions peer $ \_ -> Conversation (invReqDataConversation key value)
    propagateOne sactions (DataOnlyPM mPeer value) = do
        logDebug $ sformat ("Propagation data: "%build) value
        targets <- getTargets mPeer
        void $ Concurrent.forConcurrently (toList targets) $ \peer ->
            withConnectionTo sactions peer $ \_ -> Conversation (dataConversation value)


    dataConversation
        :: forall value .
           value
        -> ConversationActions (DataMsg value) Void m
        -> m ()
    dataConversation value conv = send conv $ DataMsg value

    invReqDataConversation
        :: forall key value .
           ( Eq key )
        => key
        -> value
        -> ConversationActions (InvOrData key value) (ReqMsg key) m
        -> m ()
    invReqDataConversation key conts conv = do
        send conv $ Left $ InvMsg key
        let whileNotK = do
              -- TODO discover the bound using some monadic computation.
              rm <- recv conv maxBound
              whenJust rm $ \ReqMsg{..} -> do
                if rmKey == key
                   then send conv $ Right $ DataMsg conts
                   else whileNotK
        whileNotK

-- | A listener for 'ReqMsg', given a way to produce a value from a key
--   within some monad. It will send back a `DataMsg` with the value if it is
--   found.
--
--   TBD why do we need this 'ReqMsg' listener? This seems to have nothing
--   to do with relaying. It's an unsolicited query for some key/value pair,
--   which induces no further relaying. Seems unrelated to relay/broadcast.
handleReqL
    :: forall packingType peerData key value m .
       ( Msg.Serializable packingType (ReqMsg key)
       , Msg.Serializable packingType (InvOrData key value)
       , Msg.Message (InvOrData key value)
       , Msg.Message (ReqMsg key)
       , Buildable key
       , WithLogger m
       )
    => (NodeId -> key -> m (Maybe value))
    -> Listener packingType peerData m
handleReqL handleReq = ListenerActionConversation $ \_ peer cactions ->
   let handlingLoop = do
           mbMsg <- recv cactions maxBound
           whenJust mbMsg $ \ReqMsg{..} -> do
               dtMB <- handleReq peer rmKey
               case dtMB of
                   Nothing -> logNoData rmKey
                   Just dt -> logHaveData rmKey >> send cactions (constructDataMsg dt)
               handlingLoop
    in handlingLoop
  where
    constructDataMsg :: value -> InvOrData key value
    constructDataMsg = Right . DataMsg
    logNoData rmKey = logDebug $ sformat
        ("We don't have data for key "%build)
        rmKey
    logHaveData rmKey= logDebug $ sformat
        ("We have data for key "%build)
        rmKey

-- | A listener for 'InvMsg'. In fact, it expects 'InvOrData' because it may
--   send a 'ReqMsg' after the initial 'InvMsg', after which it will expect
--   a 'DataMsg'.
handleInvL
  :: forall packingType peerData key value m .
     ( Msg.Message (ReqMsg key)
     , Msg.Message (InvOrData key value)
     , Msg.Serializable packingType (ReqMsg key)
     , Msg.Serializable packingType (InvOrData key value)
     , Buildable key
     , Buildable value
     , Eq key
     , WithLogger m
     , Mockable Throw m
     )
  => (PropagationMsg packingType -> m ()) -- ^ How to relay the data.
  -> InvReqDataParams key value m
  -> Listener packingType peerData m
handleInvL propagateData InvReqDataParams{..} = ListenerActionConversation $ \_ peer cactions ->
    let handlingLoop = do
            -- Expect an 'InvMsg'.
            -- Actually, this is 'InvOrData', and we give an error in case
            -- 'DataMsg' is observed first (expectInv).
            inv' <- recv cactions maxBound
            whenJust inv' $ expectInv $ \InvMsg{..} -> do
                -- 'handleInv' comes from the 'InvReqDataParams' record.
                -- Note that 'handleInvDo' returns a new key 'useful'
                -- TBD will it always be the same as 'imKey'? You'd think so...
                useful <- handleInvDo (handleInv peer) imKey
                whenJust useful $ \ne -> do
                    -- We think their 'InvMsg' key is useful so we request
                    -- the data.
                    send cactions $ ReqMsg ne
                    -- Now the pattern repeats but we expect a 'DataMsg'.
                    dt' <- recv cactions maxBound
                    whenJust dt' $ expectData $ \DataMsg{..} -> do
                          handleDataDo peer propagateData contentsToKey (handleData peer) dmContents
                          -- handlingLoop

                          -- TODO CSL-1148 Improve relaing: support multiple data
                          -- Need to receive Inv and Data messages simultaneously
                          -- Maintain state of sent Reqs
                          -- And check data we are sent is what we expect (currently not)
    in handlingLoop



-- | Make a listener for 'DataMsg', given a way to decide whether the propagate
--   the data, and possibly do something with it in the monadic context.
handleDataL
    :: forall packingType peerData value m .
       ( Msg.Serializable packingType (DataMsg value)
       , Msg.Serializable packingType Void
       , Msg.Message Void
       , Msg.Message (DataMsg value)
       , Buildable value
       , WithLogger m
       )
    => (PropagationMsg packingType -> m ()) -- ^ How to relay the data.
    -> (NodeId -> value -> m Bool) -- ^ Give 'True' to propagate, 'False' otherwise.
    -> Listener packingType peerData m
handleDataL propagateData handleData = ListenerActionConversation $ \_ peer (cactions :: ConversationActions Void (DataMsg value) m) ->
    let handlingLoop = do
            mbMsg <- recv cactions maxBound
            whenJust mbMsg $ \DataMsg{..} -> do
                ifM (handleData peer dmContents)
                    (propagateData $ constructDataOnlyPM peer dmContents)
                    (logUseless dmContents)
                handlingLoop
    in handlingLoop
  where
    constructDataOnlyPM :: NodeId -> value -> PropagationMsg packingType
    constructDataOnlyPM = DataOnlyPM . Just
    logUseless dmContents = logWarning $ sformat
        ("Ignoring data "%build) dmContents

-- | Given a value, determine its key and whether it should propagate.
handleDataDo
    :: forall packingType key value m .
       ( Buildable key
       , Eq key
       , Buildable value
       , Msg.Message (InvOrData key value)
       , Msg.Message (ReqMsg key)
       , Msg.Serializable packingType (InvOrData key value)
       , Msg.Serializable packingType (ReqMsg key)
       , WithLogger m
       )
    => NodeId -- ^ The peer which gave the data.
    -> (PropagationMsg packingType -> m ())
    -> (value -> m key)  -- ^ value determines its own key (with effects)
    -> (value -> m Bool) -- ^ value determines whether it should propagate (with effects)
    -> value
    -> m ()
handleDataDo peer propagateData contentsToKey handleData dmContents = do
    -- TBD is it important to run this effects before running 'handleData'?
    -- Seems ideal to run this only if we actually need the key (for something
    -- other than logging).
    dmKey <- contentsToKey dmContents
    ifM (handleData dmContents)
        (propagateData $ InvReqDataPM (Just peer) dmKey dmContents) $
            logDebug $ sformat
                ("Ignoring data "%build%" for key "%build) dmContents dmKey

-- | Determine whether a key is useful and do some logging.
handleInvDo
    :: forall key m .
       ( Buildable key
       , WithLogger m
       )
    => (key -> m Bool)
    -> key
    -> m (Maybe key)
handleInvDo decide imKey =
    ifM (decide imKey)
        (Just imKey <$ logUseful)
        (Nothing <$ logUseless)
  where
    logUseless = logDebug $ sformat
        ("Ignoring inv for key "%build%", because it's useless")
        imKey
    logUseful = logDebug $ sformat
        ("We'll request data for key "%build%", because it's useful")
        imKey
