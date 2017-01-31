-- | Framework for Inv/Req/Dat message handling

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Communication.Relay
       ( Relay (..)
       , InvMsg (..)
       , ReqMsg (..)
       , DataMsg (..)
       , relayListeners
       , relayStubListeners
       , RelayProxy (..)
       , InvOrData
       ) where

import qualified Data.List.NonEmpty            as NE
import           Formatting                    (build, sformat, stext, (%))
import           Mockable                      (Mockable, Throw, throw)
import           Node.Message                  (Message)
import           Serokell.Util.Text            (listJson)
import           Serokell.Util.Verify          (VerificationRes (..))
import           System.Wlog                   (logDebug, logInfo, logWarning)
import           System.Wlog                   (WithLogger)
import           Universum

import           Pos.Binary.Class              (Bi (..))
import           Pos.Communication.Protocol    (ConversationActions (..), ListenerSpec,
                                                NOP, OutSpecs, listenerConv, mergeLs)
import           Pos.Communication.Types.Relay (DataMsg (..), InvMsg (..), ReqMsg (..), InvOrData)
import           Pos.Communication.Util        (stubListenerConv)
import           Pos.Context                   (WithNodeContext (getNodeContext),
                                                ncPropagation)
import           Pos.DHT.Model.Class           (MonadDHT (..))
import           Pos.DHT.Model.Neighbors       (sendToNeighbors)
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
       , Bi (DataMsg key contents)
       , Bi (InvMsg key tag)
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
    :: ( Bi (DataMsg key contents)
       , Bi (InvMsg key tag)
       , MonadDHT m
       , Relay m tag key contents
       , WorkMode ssc m
       -- [CSL-659] remove after rewriting to conv
       , Bi NOP
       , Message NOP
       )
    => RelayProxy key tag contents
    -> DataMsg key contents
    -> m Bool
handleDataL proxy msg@(DataMsg {..}) =
    processMessage False "Data" dmContents verifyDataContents $ do
        let _ = dataCatchType proxy msg
        ifM (handleData dmContents dmKey)
            handleDataLDo $
                False <$ logDebug (sformat
                    ("Ignoring data "%build%" for key "%build) dmContents dmKey)
  where
    handleDataLDo = do
        shouldPropagate <- ncPropagation <$> getNodeContext
        if shouldPropagate then
            logInfo $ sformat
                ("Adopted data "%build%" "%
                  "for key "%build%", propagating...")
                dmContents dmKey
        else
            logInfo $ sformat
                ("Adopted data "%build%" for "%
                  "key "%build%", no propagation")
                dmContents dmKey
        pure shouldPropagate

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
     , Mockable Throw m
     , Relay m tag key contents
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
                    whenJustM recv $ expectRight $ \dt@DataMsg{..} -> do
                        prop <- handleDataL proxy dt
                        when prop $ pass

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
