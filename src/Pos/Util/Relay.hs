-- | Framework for Inv/Req/Dat message handling

module Pos.Util.Relay
       ( Relay (..)
       , InvMsg (..)
       , ReqMsg (..)
       , DataMsg (..)
       , handleInvL
       , handleReqL
       , handleDataL
       ) where

import           Control.TimeWarp.Rpc      (Message (..), messageName')
import           Data.List.NonEmpty        (NonEmpty (..))
import           Data.Proxy                (Proxy (..))
import qualified Data.Text.Buildable
import           Formatting                (build, sformat, stext, (%))
import           Serokell.Util.Text        (listJson)
import           Serokell.Util.Verify      (VerificationRes (..))
import           System.Wlog               (logDebug, logInfo, logWarning)
import           Universum

import           Pos.Binary.Class          (Bi)
import           Pos.Communication.Methods (sendToNeighborsSafe)
import           Pos.Communication.Types   (MutSocketState, ResponseMode)
import           Pos.Context               (WithNodeContext (getNodeContext),
                                            ncPropagation)
import           Pos.DHT.Model             (ListenerDHT (..), MonadDHTDialog, replyToNode)
import           Pos.Util                  (NamedMessagePart (..))
import           Pos.WorkMode              (WorkMode)
import           System.Wlog               (WithLogger)

class ( Buildable tag
      , Buildable contents
      , Buildable key
      , Typeable tag
      , Typeable contents
      , Typeable key
      , NamedMessagePart tag
      , NamedMessagePart contents
      ) => Relay m tag key contents | tag -> contents, contents -> tag, contents -> key, tag -> key where
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

-- | Inventory message. Can be used to announce the fact that you have
-- some data.
data InvMsg key tag = InvMsg
    { imTag  :: !tag
    , imKeys :: !(NonEmpty key)
    }

instance (Typeable key, Typeable tag, NamedMessagePart tag) => Message (InvMsg key tag) where
    messageName p = "Inventory " <> nMessageName (tagM p)
      where
        tagM :: Proxy (InvMsg key tag) -> Proxy tag
        tagM _ = Proxy

    formatMessage = messageName'

-- | Request message. Can be used to request data (ideally data which
-- was previously announced by inventory message).
data ReqMsg key tag = ReqMsg
    { rmTag  :: !tag
    , rmKeys :: !(NonEmpty key)
    }

instance (Typeable key, Typeable tag, NamedMessagePart tag) => Message (ReqMsg key tag) where
    messageName p = "Request " <> nMessageName (tagM p)
      where
        tagM :: Proxy (ReqMsg key tag) -> Proxy tag
        tagM _ = Proxy

    formatMessage = messageName'

-- | Data message. Can be used to send actual data.
data DataMsg key contents = DataMsg
    { dmContents :: !contents
    , dmKey      :: !key
    }

instance (Typeable key, Typeable contents, NamedMessagePart contents) => Message (DataMsg key contents) where
    messageName p = "Data " <> nMessageName (contentsM p)
      where
        contentsM :: Proxy (DataMsg key contents) -> Proxy contents
        contentsM _ = Proxy

    formatMessage = messageName'

newtype ListenersHolder ssc m tag = ListenersHolder [ListenerDHT (MutSocketState ssc) m]

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

handleInvL :: (Bi (ReqMsg key tag), Relay m tag key contents, ResponseMode ssc m) => InvMsg key tag -> m ()
handleInvL InvMsg {..} = processMessage "Inventory" imTag verifyInvTag $ do
    res <- zip (toList imKeys) <$> mapM (handleInv imTag) (toList imKeys)
    let useful = filter' identity res
        useless = filter' not res
    when (not $ null useless) $
        logDebug $ sformat
          ("Ignoring inv "%build%" for addresses "%listJson%", because they're useless")
          imTag useless
    case useful of
      []     -> pure ()
      (a:as) -> replyToNode $ ReqMsg imTag (a :| as)

filter' pred = map fst . filter (pred . snd)

handleReqL :: (Bi (DataMsg key contents), Relay m tag key contents, ResponseMode ssc m) => ReqMsg key tag -> m ()
handleReqL ReqMsg {..} = processMessage "Request" rmTag verifyReqTag $ do
    res <- zip (toList rmKeys) <$> mapM (handleReq rmTag) (toList rmKeys)
    let noDataAddrs = filter' isNothing res
        datas = catMaybes $ map (\(addr, m) -> (,addr) <$> m) res
    when (not $ null noDataAddrs) $
        logDebug $ sformat
          ("No data "%build%" for addresses "%listJson)
          rmTag noDataAddrs
    mapM_ (replyToNode . uncurry DataMsg) datas

handleDataL :: (Bi (InvMsg key tag), Relay m tag key contents, WorkMode ssc m) => DataMsg key contents -> m ()
handleDataL DataMsg {..} = processMessage "Data" dmContents verifyDataContents $ do
    ifM (handleData dmContents dmKey)
      handleDataLDo $
      logDebug $ sformat
          ("Ignoring data "%build%" for address "%build)
          dmContents dmKey
  where
    handleDataLDo =
        ifM (ncPropagation <$> getNodeContext)
            propagate $
            logInfo $ sformat
                ("Adopted data "%build%" for address "%build%", no propagation")
                dmContents dmKey
    propagate = do
        logInfo $ sformat
            ("Adopted data "%build%" for address "%build%", propagating...")
            dmContents dmKey
        tag <- contentsToTag dmContents
        sendToNeighborsSafe $ InvMsg tag (dmKey :| [])
