{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Protocol/versioning related communication types.

module Pos.Communication.Types.Protocol
       ( Action
       , ActionSpec (..)
       , checkInSpecs
       , Conversation (..)
       , convH
       , HandlerSpec (..)
       , HandlerSpecs
       , InSpecs (..)
       , Listener
       , listenerMessageName
       , ListenerSpec (..)
       , MkListeners (..)
       , notInSpecs
       , N.ConversationActions (..)
       , NodeId
       , OutSpecs (..)
       , PackingType
       , PeerId (..)
       , PeerData
       , N.Converse (..)
       , SendActions (..)
       , EnqueueMsg
       , immediateConcurrentConversations
       , enqueueMsg'
       , waitForConversations
       , toOutSpecs
       , VerInfo (..)
       , Worker
       , WorkerSpec
       , NodeType (..)
       , MsgType (..)
       , Origin (..)
       , Msg
       , MsgSubscribe (..)
       ) where

import           Data.Aeson                 (ToJSON (..), FromJSON (..), Value)
import           Data.Aeson.Types           (Parser)
import qualified Data.ByteString.Base64     as B64 (encode, decode)
import qualified Data.Map                   as M
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text.Buildable        as B
import qualified Data.Text.Encoding         as Text (encodeUtf8, decodeUtf8)
import qualified Data.Text.Internal.Builder as B
import           Formatting                 (bprint, build, hex, sformat, (%))
-- TODO should not have to import outboundqueue stuff here. MsgType and
-- NodeType should be a cardano-sl notion.
import           Pos.Network.Types          (NodeType (..), MsgType (..), Origin (..), NodeId (..))
import           Mockable.Class             (Mockable)
import           Mockable.Concurrent        (Async, async, wait)
import           Network.Transport          (EndPointAddress (..))
import qualified Node                       as N
import           Node.Message.Class         (Message (..), MessageName (..))
import           Serokell.Util.Text         (listJson, mapJson)
import           Serokell.Util.Base16       (base16F)
import           Universum

import           Pos.Binary.Class           (Bi)
import           Pos.Communication.BiP      (BiP)
import           Pos.Core.Types             (BlockVersion)

type PackingType = BiP
type PeerData = VerInfo

type Listener = N.Listener PackingType PeerData
type Worker m = Action m ()
type Action m a = SendActions m -> m a
newtype ActionSpec m a = ActionSpec (VerInfo -> Action m a)
type WorkerSpec m = ActionSpec m ()

type Msg = MsgType NodeId

data SendActions m = SendActions {
      -- | Establish a bi-direction conversation session with a node.
      --
      -- A NonEmpty of Conversations is given as a sort of multi-version
      -- handling thing. The one to use is determined by trying to match
      -- in- and out-specs using VerInfo of our node and the peer.
      --
      -- FIXME change this. Surely there is a more straightforward way.
      -- Why use in- and out-specs at all? Why not just a version number?
      withConnectionTo
          :: forall t .
             NodeId
          -> (PeerData -> NonEmpty (Conversation m t))
          -> m t
    , enqueueMsg
          :: forall t .
             Msg
          -> (NodeId -> PeerData -> NonEmpty (Conversation m t))
          -> m (Map NodeId (m t))
    }

-- | An 'EnqueueMsg m' which concurrently converses with a list of nodes.
-- This will spawn a conversation against each of them and then return.
--
-- You probably do not want to use this. Use 'enqueueMsg' instead.
immediateConcurrentConversations
    :: ( Applicative m, Mockable Async m )
    => SendActions m
    -> [NodeId]
    -> EnqueueMsg m
immediateConcurrentConversations sendActions peers _ k = do
    lst <- forM peers $ \peer -> do
        it <- async $ withConnectionTo sendActions peer $ \pd -> k peer pd
        return (peer, wait it)
    return $ M.fromList lst

type EnqueueMsg m =
       forall t .
       Msg
    -> (NodeId -> PeerData -> NonEmpty (Conversation m t))
    -> m (Map NodeId (m t))

-- | Enqueue a conversation with a bunch of peers and then wait for all of
-- the results.
enqueueMsg'
    :: forall m t .
       ( Monad m )
    => SendActions m
    -> Msg
    -> (NodeId -> PeerData -> NonEmpty (Conversation m t))
    -> m (Map NodeId t)
enqueueMsg' sendActions msg k =
    enqueueMsg sendActions msg k >>= waitForConversations

-- | Wait for enqueued conversations to complete (useful in a bind with
-- 'enqueueMsg').
waitForConversations
    :: ( Applicative m )
    => Map NodeId (m t)
    -> m (Map NodeId t)
waitForConversations = sequenceA

-- FIXME do not demand Message on rcv. That's only done for the benefit of
-- this in- and out-spec motif. See TW-152.
data Conversation m t where
    Conversation
        :: ( Bi snd, Message snd, Bi rcv, Message rcv )
        => (N.ConversationActions snd rcv m -> m t)
        -> Conversation m t

newtype PeerId = PeerId ByteString
  deriving (Eq, Ord, Show, Generic, Hashable)

instance ToJSON PeerId where
    toJSON (PeerId bs) = toJSONBS bs

instance ToJSON NodeId where
    toJSON (NodeId (EndPointAddress bs)) = toJSON (Text.decodeUtf8 (B64.encode bs))

toJSONBS :: ByteString -> Value
toJSONBS = toJSON . Text.decodeUtf8 . B64.encode

instance FromJSON PeerId where
    parseJSON = fromJSONBS PeerId

instance FromJSON NodeId where
    parseJSON = fromJSONBS (NodeId . EndPointAddress)

fromJSONBS :: (ByteString -> a) -> Value -> Parser a
fromJSONBS f v = do
    bs <- Text.encodeUtf8 <$> parseJSON v
    case B64.decode bs of
        Left err -> fail err
        Right decoded -> pure $ f decoded

instance Buildable PeerId where
    build (PeerId bs) = buildBS bs

instance Buildable NodeId where
    build (NodeId (EndPointAddress bs)) = buildBS bs

buildBS :: ByteString -> B.Builder
buildBS = bprint base16F

data HandlerSpec
    = ConvHandler { hsReplyType :: MessageName }
    | UnknownHandler Word8 ByteString
    deriving (Show, Generic, Eq)

convH :: (Message snd, Message rcv) => Proxy snd -> Proxy rcv -> (MessageName, HandlerSpec)
convH pSnd pReply = (messageName pSnd, ConvHandler $ messageName pReply)

instance Buildable HandlerSpec where
    build (ConvHandler (MessageName replyType)) =
        bprint ("Conv "%base16F) replyType
    build (UnknownHandler htype hcontent) =
        bprint ("UnknownHandler "%hex%" "%base16F) htype hcontent

instance Buildable (MessageName, HandlerSpec) where
    build (MessageName rcvType, h) = bprint (base16F % " -> " % build) rcvType h

type HandlerSpecs = HashMap MessageName HandlerSpec

instance Buildable HandlerSpecs where
    build x = bprint ("HandlerSpecs: "%listJson) (HM.toList x)

-- FIXME don't use types which contain arbitrarily big values (like
-- HandlerSpecs i.e. HashMap) because this VerInfo will have to be read in
-- from peers.
-- Why not just use a version number?
data VerInfo = VerInfo
    { vIMagic        :: Int32
    , vIBlockVersion :: BlockVersion
    , vIInHandlers   :: HandlerSpecs
    , vIOutHandlers  :: HandlerSpecs
    } deriving (Eq, Generic, Show)

instance Buildable VerInfo where
    build VerInfo {..} = bprint ("VerInfo { magic="%hex%", blockVersion="
                                %build%", inSpecs="%mapJson%", outSpecs="
                                %mapJson%"}")
                                vIMagic
                                vIBlockVersion
                                (HM.toList vIInHandlers)
                                (HM.toList vIOutHandlers)

checkInSpecs :: (MessageName, HandlerSpec) -> HandlerSpecs -> Bool
checkInSpecs (name, sp) specs = case name `HM.lookup` specs of
                              Just sp' -> sp == sp'
                              _        -> False

notInSpecs :: (MessageName, HandlerSpec) -> HandlerSpecs -> Bool
notInSpecs sp' = not . checkInSpecs sp'

-- ListenerSpec makes no sense like this. Surely the HandlerSpec must also
-- depend upon the VerInfo.
data ListenerSpec m = ListenerSpec
    { lsHandler :: VerInfo -> Listener m -- ^ Handler accepts out verInfo and returns listener
    , lsInSpec  :: (MessageName, HandlerSpec)
    }

-- | The MessageName that the listener responds to.
listenerMessageName :: forall m . Listener m -> MessageName
listenerMessageName (N.Listener (_ :: PeerData -> NodeId -> N.ConversationActions snd rcv m -> m ())) =
    messageName (Proxy @rcv)

newtype InSpecs = InSpecs HandlerSpecs
  deriving (Eq, Show, Generic)

newtype OutSpecs = OutSpecs HandlerSpecs
  deriving (Eq, Show, Generic)

instance Monoid InSpecs where
    mempty = InSpecs mempty
    (InSpecs a) `mappend` (InSpecs b) =
          InSpecs $ HM.unionWithKey merger a b
      where
        merger name h1 h2 =
          error $ sformat
              ("Conflicting key in input spec: "%build%" "%build)
              (name, h1) (name, h2)

instance Monoid OutSpecs where
    mempty = OutSpecs mempty
    (OutSpecs a) `mappend` (OutSpecs b) =
          OutSpecs $ HM.unionWithKey merger a b
      where
        merger name h1 h2 =
          if h1 == h2
             then h1
             else error $ sformat
                    ("Conflicting key output spec: "%build%" "%build)
                    (name, h1) (name, h2)

toOutSpecs :: [(MessageName, HandlerSpec)] -> OutSpecs
toOutSpecs = OutSpecs . HM.fromList

-- | Data type to represent listeners, provided upon our version info and peerData
-- received from other node, in and out specs for all listeners which may be provided
data MkListeners m = MkListeners
        { mkListeners :: VerInfo -> PeerData -> [Listener m]
        -- ^ Accepts our version info and their peerData and returns set of listeners
        , inSpecs     :: InSpecs
        -- ^ Aggregated specs for what we accept on incoming connections
        , outSpecs    :: OutSpecs
        -- ^ Aggregated specs for which outgoing connections we might initiate
        }

instance Monad m => Monoid (MkListeners m) where
    mempty = MkListeners (\_ _ -> []) mempty mempty
    a `mappend` b = MkListeners act (inSpecs a `mappend` inSpecs b) (outSpecs a `mappend` outSpecs b)
      where
        act vI pD = (++) (mkListeners a vI pD) (mkListeners b vI pD)

-- | 'Subscribe' message
--
-- This can be used by behind-NAT nodes to subscribe to the 'OutboundQueue'
-- of a relay node. The node will remain subscribed for the duration of the
-- conversation.
data MsgSubscribe = MsgSubscribe
    deriving (Generic, Show, Eq)
