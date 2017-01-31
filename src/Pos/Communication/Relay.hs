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
       ) where

import           Formatting                    (build, sformat, stext, (%))
import           Node.Message                  (Message)
import           Serokell.Util.Text            (listJson)
import           Serokell.Util.Verify          (VerificationRes (..))
import           System.Wlog                   (logDebug, logInfo, logWarning)
import           System.Wlog                   (WithLogger)
import           Universum

import           Pos.Binary.Class              (Bi (..))
import           Pos.Communication.Protocol    (ListenerSpec, NOP, OutSpecs,
                                                SendActions (..), listenerOneMsg, mergeLs,
                                                oneMsgH, sendTo, toOutSpecs)
import           Pos.Communication.Types.Relay (DataMsg (..), InvMsg (..), ReqMsg (..))
import           Pos.Communication.Util        (stubListenerOneMsg)
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
      , Message (InvMsg key tag)
      , Message (ReqMsg key tag)
      , Message (DataMsg key contents)
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

processMessage
  :: (Buildable param, WithLogger m)
  => Text -> param -> (param -> m VerificationRes) -> m () -> m ()
processMessage name param verifier action = do
    verRes <- verifier param
    case verRes of
      VerSuccess -> action
      VerFailure reasons ->
          logWarning $ sformat
            ("Wrong "%stext%": invalid "%build%": "%listJson)
            name param reasons

filterSecond :: (b -> Bool) -> [(a,b)] -> [a]
filterSecond predicate = map fst . filter (predicate . snd)

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
    -> (ListenerSpec m, OutSpecs)
handleInvL proxy = listenerOneMsg outSpecs $
  \_ peerId sendActions msg@(InvMsg {..}) -> do
      let _ = invCatchType proxy msg
      processMessage "Inventory" imTag verifyInvTag $ do
          res <- zip (toList imKeys) <$> mapM (handleInv imTag) (toList imKeys)
          let useful = filterSecond identity res
              useless = filterSecond not res
          when (not $ null useless) $
              logDebug $ sformat
                ("Ignoring inv "%build%" for keys "%listJson%", because they're useless")
                imTag useless
          case useful of
            []     -> pure ()
            (a:as) -> sendTo sendActions peerId $ ReqMsg imTag (a :| as)
  where
    outSpecs = toOutSpecs [ oneMsgH (reqMsgProxy proxy) ]

handleReqL
    :: ( Bi (ReqMsg key tag)
       , Bi (DataMsg key contents)
       , Relay m tag key contents
       , MinWorkMode m
       -- [CSL-659] remove after rewriting to conv
       , Bi NOP
       , Message NOP
       )
    => RelayProxy key tag contents
    -> (ListenerSpec m, OutSpecs)
handleReqL proxy = listenerOneMsg outSpecs $
  \_ peerId sendActions msg@(ReqMsg {..}) -> do
      let _ = reqCatchType proxy msg
      processMessage "Request" rmTag verifyReqTag $ do
          res <- zip (toList rmKeys) <$> mapM (handleReq rmTag) (toList rmKeys)
          let noDataAddrs = filterSecond isNothing res
              datas = catMaybes $ map (\(addr, m) -> (,addr) <$> m) res
          when (not $ null noDataAddrs) $
              logDebug $ sformat
                  ("No data "%build%" for keys "%listJson)
                  rmTag noDataAddrs
          mapM_ ((sendTo sendActions peerId) . uncurry DataMsg) datas
  where
    outSpecs = toOutSpecs [ oneMsgH (dataMsgProxy proxy) ]

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
    -> (ListenerSpec m, OutSpecs)
handleDataL proxy = listenerOneMsg outSpecs $
  \_ _ sendActions msg@(DataMsg {..}) -> do
      let _ = dataCatchType proxy msg
          handleDataLDo = do
              shouldPropagate <- ncPropagation <$> getNodeContext
              if shouldPropagate then do
                  logInfo $ sformat
                      ("Adopted data "%build%" "%
                       "for key "%build%", propagating...")
                      dmContents dmKey
                  tag <- contentsToTag dmContents
                  sendToNeighbors sendActions $ InvMsg tag (one dmKey)
              else do
                  logInfo $ sformat
                      ("Adopted data "%build%" for "%
                       "key "%build%", no propagation")
                      dmContents dmKey
      processMessage "Data" dmContents verifyDataContents $
          ifM (handleData dmContents dmKey)
              handleDataLDo $
              logDebug $ sformat
                  ("Ignoring data "%build%" for key "%build)
                  dmContents dmKey
  where
    outSpecs = toOutSpecs [ oneMsgH (invMsgProxy proxy) ]

data RelayProxy key tag contents = RelayProxy

invCatchType :: RelayProxy key tag contents -> InvMsg key tag -> ()
invCatchType _ _ = ()
invMsgProxy :: RelayProxy key tag contents -> Proxy (InvMsg key tag)
invMsgProxy _ = Proxy

reqCatchType :: RelayProxy key tag contents -> ReqMsg key tag -> ()
reqCatchType _ _ = ()
reqMsgProxy :: RelayProxy key tag contents -> Proxy (ReqMsg key tag)
reqMsgProxy _ = Proxy

dataCatchType :: RelayProxy key tag contents -> DataMsg key contents -> ()
dataCatchType _ _ = ()
dataMsgProxy :: RelayProxy key tag contents -> Proxy (DataMsg key contents)
dataMsgProxy _ = Proxy

relayListeners
  :: ( MonadDHT m
     , Bi (InvMsg key tag)
     , Bi (ReqMsg key tag)
     , Bi (DataMsg key contents)
     , Relay m tag key contents
     , WithLogger m
     , WorkMode ssc m
       -- [CSL-659] remove after rewriting to conv
     , Bi NOP
     , Message NOP
     )
  => RelayProxy key tag contents -> ([ListenerSpec m], OutSpecs)
relayListeners proxy = mergeLs $ map ($ proxy) [handleInvL, handleReqL, handleDataL]

relayStubListeners
    :: ( WithLogger m
       , Bi (InvMsg key tag)
       , Bi (ReqMsg key tag)
       , Bi (DataMsg key contents)
       , Message (InvMsg key tag)
       , Message (ReqMsg key tag)
       , Message (DataMsg key contents)
       )
    => RelayProxy key tag contents -> ([ListenerSpec m], OutSpecs)
relayStubListeners p = mergeLs
    [ stubListenerOneMsg $ invMsgProxy p
    , stubListenerOneMsg $ reqMsgProxy p
    , stubListenerOneMsg $ dataMsgProxy p
    ]
