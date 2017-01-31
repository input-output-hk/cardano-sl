-- | Framework for Inv/Req/Dat message handling

{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Communication.Relay
       ( Relay (..)
       , InvMsg (..)
       , ReqMsg (..)
       , DataMsg (..)
       , relayListeners
       , relayStubListeners
       , relayWorkers
       , RelayProxy (..)
       , InvOrData

       , invReqDataFlow
       ) where

import           Control.Concurrent.STM        (isFullTBQueue, readTBQueue, writeTBQueue)
import qualified Data.List.NonEmpty            as NE
import           Formatting                    (build, sformat, shown, stext, (%))
import           Mockable                      (Mockable, Throw, handleAll, throw, throw)
import           Node.Message                  (Message)
import           Serokell.Util.Text            (listJson)
import           Serokell.Util.Verify          (VerificationRes (..))
import           System.Wlog                   (WithLogger, logDebug, logError, logInfo,
                                                logWarning)
import           Universum

import           Pos.Binary.Class              (Bi (..))
import           Pos.Communication.Message     (MessagePart)
import           Pos.Communication.Protocol    (ConversationActions (..), ListenerSpec,
                                                NOP, OutSpecs, SendActions (..),
                                                WorkerSpec, listenerConv, mergeLs, worker)
import           Pos.Communication.Specs       (allOutSpecs)
import           Pos.Communication.Types.Relay (DataMsg (..), InvMsg (..), InvOrData,
                                                ReqMsg (..))
import           Pos.Communication.Util        (stubListenerConv)
import           Pos.Context                   (NodeContext (..), SomeInvMsg (..),
                                                WithNodeContext (getNodeContext),
                                                ncPropagation)
import           Pos.DHT.Model                 (DHTNode)
import           Pos.DHT.Model.Class           (MonadDHT (..))
import           Pos.DHT.Model.Neighbors       (converseToNeighbors, converseToNode)
import           Pos.WorkMode                  (MinWorkMode, WorkMode)

-- | Typeclass for general Inv/Req/Dat framework. It describes monads,
-- that store data described by tag, where "key" stands for node
-- identifier.
class ( Buildable tag
      , Buildable contents
      , Buildable key
      , Typeable tag
      , Typeable contents
      , Typeable key
      , Message (InvOrData tag key contents)
      , Message (ReqMsg key tag)
      ) => Relay m tag key contents
      | tag -> contents, contents -> tag, contents -> key, tag -> key where
    -- | Converts data to tag. Tag returned in monad `m`
    -- for only type matching reason (multiparam type classes are tricky)
    contentsToTag :: contents -> m tag

    verifyInvTag :: tag -> m VerificationRes
    verifyReqTag :: tag -> m VerificationRes
    verifyDataContents :: contents -> m VerificationRes

    -- | Handle inv msg and return whether it's useful or not
    handleInv :: tag -> key -> m Bool

    -- | Handle req msg and return (Just data) in case requested data can be provided
    handleReq :: tag -> key -> m (Maybe contents)

    -- | Handle data msg and return True if message is to be propagated
    handleData :: contents -> key -> m Bool

data RelayProxy key tag contents = RelayProxy

data RelayError = UnexpectedInv
                | UnexpectedData
                | InvalidPropagationElement
  deriving (Generic, Show)

instance Exception RelayError

-- Returns useful keys.
handleInvL
    :: ( Bi (InvMsg key tag)
       , Bi (ReqMsg key tag)
       , Relay m tag key contents
       , MinWorkMode m
       -- [CSL-659] remove after rewriting to conv
       , Bi NOP
       , Message NOP
       )
    => RelayProxy key tag contents
    -> InvMsg key tag
    -> m [key]
handleInvL proxy msg@(InvMsg{..}) =
    processMessage [] "Inventory" imTag verifyInvTag $ do
        let _ = invCatchType proxy msg
        res <- zip (toList imKeys) <$> mapM (handleInv imTag) (toList imKeys)
        let useful = filterSecond identity res
            useless = filterSecond not res
        when (not $ null useless) $
            logDebug $ sformat
              ("Ignoring inv "%build%" for keys "%listJson%", because they're useless")
              imTag useless
        pure useful

handleReqL
    :: forall key tag contents m .
       ( Bi (ReqMsg key tag)
       , Bi (InvOrData tag key contents)
       , Relay m tag key contents
       , MinWorkMode m
       -- [CSL-659] remove after rewriting to conv
       , Bi NOP
       , Message NOP
       )
    => RelayProxy key tag contents
    -> (ListenerSpec m, OutSpecs)
handleReqL proxy = listenerConv $
  \_ __peerId ConversationActions{..} ->
    whenJustM recv $ \msg@(ReqMsg {..}) -> do
      let _ = reqCatchType proxy msg
      processMessage () "Request" rmTag verifyReqTag $ do
          res <- zip (toList rmKeys) <$> mapM (handleReq rmTag) (toList rmKeys)
          let noDataAddrs = filterSecond isNothing res
              datas = catMaybes $ map (\(addr, m) -> (,addr) <$> m) res
          when (not $ null noDataAddrs) $
              logDebug $ sformat
                  ("No data "%build%" for keys "%listJson)
                  rmTag noDataAddrs
          mapM_ (send . constructDataMsg) datas
  where
    constructDataMsg :: (contents, key) -> InvOrData tag key contents
    constructDataMsg = Right . uncurry DataMsg

-- Returns True if we should propagate.
handleDataL
      :: forall tag key contents ssc m .
      ( Bi (InvMsg key tag)
      , Bi (ReqMsg key tag)
      , Bi (DataMsg key contents)
      , MonadDHT m
      , MessagePart tag
      , MessagePart contents
      , Relay m tag key contents
      , WorkMode ssc m
      -- [CSL-659] remove after rewriting to conv
      , Bi NOP
      , Message NOP
      )
    => RelayProxy key tag contents
    -> DataMsg key contents
    -> m ()
handleDataL proxy msg@(DataMsg {..}) =
    processMessage () "Data" dmContents verifyDataContents $ do
        let _ = dataCatchType proxy msg
        ifM (handleData dmContents dmKey)
            handleDataLDo $
                logDebug $ sformat
                    ("Ignoring data "%build%" for key "%build) dmContents dmKey
  where
    handleDataLDo = do
        shouldPropagate <- ncPropagation <$> getNodeContext
        if shouldPropagate then do
            tag <- contentsToTag dmContents
            let inv :: InvOrData tag key contents
                inv = Left $ InvMsg tag (one dmKey)
            addToRelayQueue inv
            logInfo $ sformat
                ("Adopted data "%build%" "%
                  "for key "%build%", data has been pushed to propagation queue...")
                dmContents dmKey
        else
            logInfo $ sformat
                ("Adopted data "%build%" for "%
                  "key "%build%", no propagation")
                dmContents dmKey

processMessage
  :: (Buildable param, WithLogger m)
  => a -> Text -> param -> (param -> m VerificationRes) -> m a -> m a
processMessage defaultRes name param verifier action = do
    verRes <- verifier param
    case verRes of
      VerSuccess -> action
      VerFailure reasons ->
          defaultRes <$
              logWarning (sformat
                ("Wrong "%stext%": invalid "%build%": "%listJson)
                name param reasons)

relayListeners
  :: ( MonadDHT m
     , Bi (InvMsg key tag)
     , Bi (ReqMsg key tag)
     , Bi (DataMsg key contents)
     , Bi (InvOrData tag key contents)
     , MessagePart contents
     , MessagePart tag
     , Relay m tag key contents
     , Mockable Throw m
     , WithLogger m
     , WorkMode ssc m
       -- [CSL-659] remove after rewriting to conv
     , Bi NOP
     , Message NOP
     )
  => RelayProxy key tag contents -> ([ListenerSpec m], OutSpecs)
relayListeners proxy = mergeLs [handleReqL proxy, invDataListener]
  where
    invDataListener = listenerConv $ \_ __peerId
        (ConversationActions{..}::(ConversationActions
                                     (ReqMsg key tag)
                                     (InvOrData tag key contents)
                                     m)
        ) ->
            whenJustM recv $ expectLeft $ \inv@InvMsg{..} -> do
                useful <- handleInvL proxy inv
                whenJust (NE.nonEmpty useful) $ \ne -> do
                    send $ ReqMsg imTag ne
                    whenJustM recv $ expectRight $ \dt@DataMsg{..} -> handleDataL proxy dt

    expectLeft call (Left msg) = call msg
    expectLeft _ (Right _)     = throw UnexpectedData

    expectRight _ (Left _)       = throw UnexpectedInv
    expectRight call (Right msg) = call msg


relayStubListeners
    :: ( WithLogger m
       , Bi (InvMsg key tag)
       , Bi (ReqMsg key tag)
       , Bi (DataMsg key contents)
       , Message (InvOrData tag key contents)
       , Message (ReqMsg key tag)
       )
    => RelayProxy key tag contents -> ([ListenerSpec m], OutSpecs)
relayStubListeners p = mergeLs
    [ stubListenerConv $ invDataMsgProxy p
    , stubListenerConv $ reqMsgProxy p
    ]

filterSecond :: (b -> Bool) -> [(a,b)] -> [a]
filterSecond predicate = map fst . filter (predicate . snd)


invCatchType :: RelayProxy key tag contents -> InvMsg key tag -> ()
invCatchType _ _ = ()

reqCatchType :: RelayProxy key tag contents -> ReqMsg key tag -> ()
reqCatchType _ _ = ()
reqMsgProxy :: RelayProxy key tag contents
            -> Proxy (InvOrData tag key contents, ReqMsg key tag)
reqMsgProxy _ = Proxy

dataCatchType :: RelayProxy key tag contents -> DataMsg key contents -> ()
dataCatchType _ _ = ()

invDataMsgProxy :: RelayProxy key tag contents
                -> Proxy (ReqMsg key tag, InvOrData tag key contents)
invDataMsgProxy _ = Proxy


addToRelayQueue :: forall tag key contents ssc m .
                ( Bi (InvOrData tag key contents)
                , Bi (ReqMsg key tag)
                , Message (InvOrData tag key contents)
                , Message (ReqMsg key tag)
                , Buildable tag, Buildable key
                , WorkMode ssc m, Bi NOP, Message NOP)
                => InvOrData tag key contents -> m ()
addToRelayQueue inv = do
    queue <- ncInvPropagationQueue <$> getNodeContext
    isFull <- atomically $ isFullTBQueue queue
    if isFull then
        logWarning $ "Propagation queue is full, no propagation"
    else
        atomically $ writeTBQueue queue (SomeInvMsg inv)

relayWorkers :: forall ssc m .
             ( Mockable Throw m
             , WorkMode ssc m
             , Bi NOP, Message NOP)
             => ([WorkerSpec m], OutSpecs)
relayWorkers = first (:[]) $ worker allOutSpecs $
  \sendActions -> handleAll handleWE $ do
    queue <- ncInvPropagationQueue <$> getNodeContext
    forever $ do
        inv <- atomically $ readTBQueue queue
        case inv of
            SomeInvMsg (i@(Left (InvMsg{..}))) -> do
                logDebug $
                    sformat ("Propagation data with keys: "%listJson%" and tag: "%build) imKeys imTag
                converseToNeighbors sendActions (convHandler i)
            SomeInvMsg (Right _) -> throw InvalidPropagationElement
  where
    convHandler inv __peerId
        (ConversationActions{..}::
        (ConversationActions (InvOrData tag1 key1 contents1) (ReqMsg key1 tag1) m)) = send inv
    handleWE e = do
        logError $ sformat ("relayWorker: error caught "%shown) e
        throw e

----------------------------------------------------------------------------
-- Helpers for Communication.Methods
----------------------------------------------------------------------------

invReqDataFlow :: ( Message (InvOrData tag id contents)
                  , Message (ReqMsg id tag)
                  , Buildable id
                  , MinWorkMode m
                  , Bi tag, Bi id
                  , Bi (InvOrData tag id contents)
                  , Bi (ReqMsg id tag))
               => Text -> SendActions m -> DHTNode -> tag -> id -> contents -> m ()
invReqDataFlow what sendActions addr tag id dt = handleAll handleE $
    converseToNode sendActions addr convHandler
  where
    convHandler _
      ca@(ConversationActions{..}::(ConversationActions (InvOrData tag id contents) (ReqMsg id tag)) m) = do
        send $ Left $ InvMsg tag (one id)
        recv >>= maybe (handleE ("node didn't reply by ReqMsg"::Text)) (replyByData ca)
    replyByData ca (ReqMsg _ _) = do
      send ca $ Right $ DataMsg dt id
    handleE e =
      logWarning $
        sformat ("Error sending"%stext%", id = "%build%" to "%shown%": "%shown) what id addr e
