-- | Framework for Inv/Req/Dat message handling

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Util.Relay
       ( Relay (..)
       , InvMsg (..)
       , ReqMsg (..)
       , DataMsg (..)
       , handleInvL
       , handleReqL
       , handleDataL
       ) where

import qualified Data.ByteString.Char8   as BC
import           Data.Proxy              (Proxy (..))
import qualified Data.Text.Buildable     as B
import           Formatting              (bprint, build, sformat, stext, (%))
import           Node                    (NodeId (..), SendActions (..), sendTo)
import           Node.Message            (Message (..), MessageName (..))
import           Serokell.Util.Text      (listJson)
import           Serokell.Util.Verify    (VerificationRes (..))
import           System.Wlog             (logDebug, logInfo, logWarning)
import           System.Wlog             (WithLogger)
import           Test.QuickCheck         (Arbitrary (..))
import           Universum

import           Pos.Binary.Class        (Bi (..))
import           Pos.Communication.BiP   (BiP (..))
import           Pos.Context             (WithNodeContext (getNodeContext), ncPropagation)
import           Pos.DHT.Model.Class     (MonadDHT (..))
import           Pos.DHT.Model.Neighbors (sendToNeighbors)
import           Pos.Util                (NamedMessagePart (..))
import           Pos.WorkMode            (WorkMode)

-- | Typeclass for general Inv/Req/Dat framework. It describes monads,
-- that store data described by tag, where "key" stands for node
-- identifier.
class ( Buildable tag
      , Buildable contents
      , Buildable key
      , Typeable tag
      , Typeable contents
      , Typeable key
      , NamedMessagePart tag
      , NamedMessagePart contents
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

-- | Inventory message. Can be used to announce the fact that you have
-- some data.
data InvMsg key tag = InvMsg
    { imTag  :: !tag
    , imKeys :: !(NonEmpty key)
    }

deriving instance (Show key, Show tag) => Show (InvMsg key tag)
deriving instance (Eq key, Eq tag) => Eq (InvMsg key tag)
instance (Arbitrary key, Arbitrary tag) => Arbitrary (InvMsg key tag) where
    arbitrary = InvMsg <$> arbitrary <*> arbitrary

instance (NamedMessagePart tag) =>
         Message (InvMsg key tag) where
    messageName p = MessageName $ BC.pack "Inventory " <> encodeUtf8 (nMessageName $ tagM p)
      where
        tagM :: Proxy (InvMsg key tag) -> Proxy tag
        tagM _ = Proxy
    formatMessage _ = "Inventory"

-- | Request message. Can be used to request data (ideally data which
-- was previously announced by inventory message).
data ReqMsg key tag = ReqMsg
    { rmTag  :: !tag
    , rmKeys :: !(NonEmpty key)
    }

deriving instance (Show key, Show tag) => Show (ReqMsg key tag)
deriving instance (Eq key, Eq tag) => Eq (ReqMsg key tag)
instance (Arbitrary key, Arbitrary tag) => Arbitrary (ReqMsg key tag) where
    arbitrary = ReqMsg <$> arbitrary <*> arbitrary

instance (NamedMessagePart tag) =>
         Message (ReqMsg key tag) where
    messageName p = MessageName $ BC.pack "Request " <> encodeUtf8 (nMessageName $ tagM p)
      where
        tagM :: Proxy (ReqMsg key tag) -> Proxy tag
        tagM _ = Proxy
    formatMessage _ = "Request"

-- | Data message. Can be used to send actual data.
data DataMsg key contents = DataMsg
    { dmContents :: !contents
    , dmKey      :: !key
    }

deriving instance (Show key, Show contents) => Show (DataMsg key contents)
deriving instance (Eq key, Eq contents) => Eq (DataMsg key contents)

instance (Buildable key, Buildable contents) => Buildable (DataMsg key contents) where
    build (DataMsg key contents) =
        bprint ("key = "%build%", contents = "%build) key contents

instance (NamedMessagePart contents) =>
         Message (DataMsg key contents) where
    messageName p = MessageName $ BC.pack "Data " <> encodeUtf8 (nMessageName $ contentsM p)
      where
        contentsM :: Proxy (DataMsg key contents) -> Proxy contents
        contentsM _ = Proxy
    formatMessage _ = "Data"

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
    :: forall m key tag contents.
       ( Bi (ReqMsg key tag)
       , Relay m tag key contents
       , WithLogger m
       )
    => InvMsg key tag
    -> NodeId
    -> SendActions BiP m
    -> m ()
handleInvL InvMsg {..} peerId sendActions = processMessage "Inventory" imTag verifyInvTag $ do
    res <- zip (toList imKeys) <$> mapM (handleInv imTag) (toList imKeys)
    let useful = filterSecond identity res
        useless = filterSecond not res
    when (not $ null useless) $
        logDebug $ sformat
          ("Ignoring inv "%build%" for addresses "%listJson%", because they're useless")
          imTag useless
    case useful of
      []     -> pure ()
      (a:as) -> sendTo sendActions peerId $ ReqMsg imTag (a :| as)

handleReqL
    :: forall m key tag contents.
       ( Bi (DataMsg key contents)
       , Relay m tag key contents
       , WithLogger m
       )
    => ReqMsg key tag
    -> NodeId
    -> SendActions BiP m
    -> m ()
handleReqL ReqMsg {..} peerId sendActions = processMessage "Request" rmTag verifyReqTag $ do
    res <- zip (toList rmKeys) <$> mapM (handleReq rmTag) (toList rmKeys)
    let noDataAddrs = filterSecond isNothing res
        datas = catMaybes $ map (\(addr, m) -> (,addr) <$> m) res
    when (not $ null noDataAddrs) $
        logDebug $ sformat
            ("No data "%build%" for addresses "%listJson)
            rmTag noDataAddrs
    mapM_ ((sendTo sendActions peerId) . uncurry DataMsg) datas

handleDataL
    :: forall ssc m key tag contents.
       ( MonadDHT m
       , Bi (InvMsg key tag)
       , Relay m tag key contents
       , WithLogger m
       , WorkMode ssc m
       )
    => DataMsg key contents
    -> NodeId
    -> SendActions BiP m
    -> m ()
handleDataL DataMsg {..} _ sendActions =
    processMessage "Data" dmContents verifyDataContents $
    ifM (handleData dmContents dmKey)
        handleDataLDo $
        logDebug $ sformat
            ("Ignoring data "%build%" for address "%build)
            dmContents dmKey
  where
    handleDataLDo = do
        shouldPropagate <- ncPropagation <$> getNodeContext
        if shouldPropagate then do
            logInfo $ sformat
                ("Adopted data "%build%" "%
                 "for address "%build%", propagating...")
                dmContents dmKey
            tag <- contentsToTag dmContents
            -- [CSL-514] TODO Log long acting sends
            sendToNeighbors sendActions $ InvMsg tag (one dmKey)
        else do
            logInfo $ sformat
                ("Adopted data "%build%" for "%
                 "address "%build%", no propagation")
                dmContents dmKey
