{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TupleSections          #-}
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
import           Pos.Types                 (StakeholderId)
import           Pos.Util                  (NamedMessagePart (..))
import           Pos.WorkMode              (WorkMode)
import           System.Wlog               (WithLogger)

class ( Buildable tag
      , Buildable contents
      , Typeable tag
      , Typeable contents
      , NamedMessagePart tag
      , NamedMessagePart contents
      ) => Relay m tag contents | tag -> contents, contents -> tag where
    -- | Converts data to tag. Tag returned in monad `m`
    -- for only type matching reason (multiparam type classes are tricky)
    contentsToTag :: contents -> m tag

    verifyInvTag :: tag -> m VerificationRes

    verifyReqTag :: tag -> m VerificationRes

    verifyDataContents :: contents -> m VerificationRes

    -- | Handle inv msg and return whether it's useful or not
    handleInv :: tag -> StakeholderId -> m Bool

    -- | Handle req msg and return (Just data) in case requested data can be provided
    handleReq :: tag -> StakeholderId -> m (Maybe contents)

    -- | Handle data msg and return True if message is to be propagated
    handleData :: contents -> StakeholderId -> m Bool

-- | Inventory message. Can be used to announce the fact that you have
-- some data.
data InvMsg tag = InvMsg
    { imTag          :: !tag
    , imStakeholders :: !(NonEmpty StakeholderId)
    }

instance (Typeable tag, NamedMessagePart tag) => Message (InvMsg tag) where
    messageName p = "Inventory " <> nMessageName (tagM p)
      where
        tagM :: Proxy (InvMsg tag) -> Proxy tag
        tagM _ = Proxy

    formatMessage = messageName'

-- | Request message. Can be used to request data (ideally data which
-- was previously announced by inventory message).
data ReqMsg tag = ReqMsg
    { rmTag          :: !tag
    , rmStakeholders :: !(NonEmpty StakeholderId)
    }

instance (Typeable tag, NamedMessagePart tag) => Message (ReqMsg tag) where
    messageName p = "Request " <> nMessageName (tagM p)
      where
        tagM :: Proxy (ReqMsg tag) -> Proxy tag
        tagM _ = Proxy

    formatMessage = messageName'

-- | Data message. Can be used to send actual data.
data DataMsg contents = DataMsg
    { dmContents    :: !contents
    , dmStakeholder :: !StakeholderId
    }

instance (Typeable contents, NamedMessagePart contents) => Message (DataMsg contents) where
    messageName p = "Data " <> nMessageName (contentsM p)
      where
        contentsM :: Proxy (DataMsg contents) -> Proxy contents
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

handleInvL :: (Bi (ReqMsg tag), Relay m tag contents, ResponseMode ssc m) => InvMsg tag -> m ()
handleInvL InvMsg {..} = processMessage "Inventory" imTag verifyInvTag $ do
    res <- zip (toList imStakeholders) <$> mapM (handleInv imTag) (toList imStakeholders)
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

handleReqL :: (Bi (DataMsg contents), Relay m tag contents, ResponseMode ssc m) => ReqMsg tag -> m ()
handleReqL ReqMsg {..} = processMessage "Request" rmTag verifyReqTag $ do
    res <- zip (toList rmStakeholders) <$> mapM (handleReq rmTag) (toList rmStakeholders)
    let noDataAddrs = filter' isNothing res
        datas = catMaybes $ map (\(addr, m) -> (,addr) <$> m) res
    when (not $ null noDataAddrs) $
        logDebug $ sformat
          ("No data "%build%" for addresses "%listJson)
          rmTag noDataAddrs
    mapM_ (replyToNode . uncurry DataMsg) datas

handleDataL :: (Bi (InvMsg tag), Relay m tag contents, WorkMode ssc m) => DataMsg contents -> m ()
handleDataL DataMsg {..} = processMessage "Data" dmContents verifyDataContents $ do
    ifM (handleData dmContents dmStakeholder)
      handleDataLDo $
      logDebug $ sformat
          ("Ignoring data "%build%" for address "%build)
          dmContents dmStakeholder
  where
    handleDataLDo =
        ifM (ncPropagation <$> getNodeContext)
            propagate $
            logInfo $ sformat
                ("Adopted data "%build%" for address "%build%", no propagation")
                dmContents dmStakeholder
    propagate = do
        logInfo $ sformat
            ("Adopted data "%build%" for address "%build%", propagating...")
            dmContents dmStakeholder
        tag <- contentsToTag dmContents
        sendToNeighborsSafe $ InvMsg tag (dmStakeholder :| [])
