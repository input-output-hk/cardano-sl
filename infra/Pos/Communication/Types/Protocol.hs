{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

-- | Protocol/versioning related communication types.

module Pos.Communication.Types.Protocol
       ( checkInSpecs
       , Conversation (..)
       , convH
       , HandlerSpec (..)
       , HandlerSpecs
       , InSpecs (..)
       , Listener
       , listenerMessageCode
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
       , enqueueMsg'
       , waitForDequeues
       , waitForConversations
       , toOutSpecs
       , VerInfo (..)
       , NodeType (..)
       , MsgType (..)
       , Origin (..)
       , Msg
       , MsgSubscribe (..)
       , MsgSubscribe1 (..)
       , mlMsgSubscribe
       , mlMsgSubscribe1
       , recvLimited
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), Value)
import           Data.Aeson.Types (Parser)
import qualified Data.ByteString.Base64 as B64 (decode, encode)
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import           Control.Exception (throwIO)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Buildable as B
import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)
import qualified Data.Text.Internal.Builder as B
import           Formatting (bprint, build, hex, sformat, shown, (%))
import qualified Network.Broadcast.OutboundQueue as OQ
import           Network.Transport (EndPointAddress (..))
import qualified Node as N
import           Node.Message.Class (Message (..), MessageCode)
import           Serokell.Util.Base16 (base16F)
import           Serokell.Util.Text (listJson, mapJson)

import           Pos.Binary.Class (Bi)
import           Pos.Communication.BiP (BiP)
import           Pos.Communication.Limits.Types (Limit (..))
import           Pos.Core.Update (BlockVersion)
import           Pos.Network.Types (MsgType (..), NodeId (..), NodeType (..), Origin (..))
import           Pos.Util.Util (toAesonError)

type PackingType = BiP
type PeerData = VerInfo

type Listener = N.Listener PackingType PeerData

type Msg = MsgType NodeId

data SendActions = SendActions {
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
          -> (PeerData -> NonEmpty (Conversation t))
          -> IO t

    , enqueueMsg
          :: forall t .
             Msg
          -> (NodeId -> PeerData -> NonEmpty (Conversation t))
          -- TOOD may as well change this type while we're at it, to include
          -- the TVar.
          -> IO (Map NodeId (IO t))
    }

type EnqueueMsg =
       forall t .
       Msg
    -> (NodeId -> PeerData -> NonEmpty (Conversation t))
    -> IO (Map NodeId (STM.TVar (OQ.PacketStatus t)))

-- | Enqueue a conversation with a bunch of peers and then wait for all of
-- the results.
enqueueMsg'
    :: forall t .
       SendActions
    -> Msg
    -> (NodeId -> PeerData -> NonEmpty (Conversation t))
    -> IO (Map NodeId t)
enqueueMsg' sendActions msg k =
    enqueueMsg sendActions msg k >>= waitForConversations

waitForDequeues
    :: forall m t .
       ( MonadIO m )
    => Map NodeId (STM.TVar (OQ.PacketStatus t))
    -> Map NodeId (m t)
waitForDequeues = fmap waitOne

waitOne :: forall m t . (MonadIO m) => STM.TVar (OQ.PacketStatus t) -> m t
waitOne statusVar = liftIO . join . atomically $ do
    st <- readTVar statusVar
    case st of
        OQ.PacketEnqueued        -> STM.retry
        OQ.PacketAborted         -> pure (throwIO OQ.Aborted)
        OQ.PacketDequeued thread -> pure (Async.wait thread)

-- | Wait for enqueued conversations to complete (useful in a bind with
-- 'enqueueMsg').
waitForConversations
    :: Map NodeId (IO t)
    -> IO (Map NodeId t)
waitForConversations = sequenceA

-- FIXME do not demand Message on rcv. That's only done for the benefit of
-- this in- and out-spec motif. See TW-152.
data Conversation t where
    Conversation
        :: ( Bi snd, Message snd, Bi rcv, Message rcv )
        => (N.ConversationActions snd rcv -> IO t)
        -> Conversation t

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
    toAesonError . bimap fromString f $ B64.decode bs

instance Buildable PeerId where
    build (PeerId bs) = buildBS bs

instance Buildable NodeId where
    build (NodeId (EndPointAddress bs)) = bprint shown bs

buildBS :: ByteString -> B.Builder
buildBS = bprint base16F

data HandlerSpec
    = ConvHandler { hsReplyType :: MessageCode }
    | UnknownHandler Word8 ByteString
    deriving (Show, Generic, Eq)

convH :: (Message snd, Message rcv) => Proxy snd -> Proxy rcv -> (MessageCode, HandlerSpec)
convH pSnd pReply = (messageCode pSnd, ConvHandler $ messageCode pReply)

instance Buildable HandlerSpec where
    build (ConvHandler replyType) =
        bprint ("Conv "%hex) replyType
    build (UnknownHandler htype hcontent) =
        bprint ("UnknownHandler "%hex%" "%base16F) htype hcontent

instance Buildable (MessageCode, HandlerSpec) where
    build (rcvType, h) = bprint (hex % " -> " % build) rcvType h

type HandlerSpecs = HashMap MessageCode HandlerSpec

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

checkInSpecs :: (MessageCode, HandlerSpec) -> HandlerSpecs -> Bool
checkInSpecs (name, sp) specs = case name `HM.lookup` specs of
                              Just sp' -> sp == sp'
                              _        -> False

notInSpecs :: (MessageCode, HandlerSpec) -> HandlerSpecs -> Bool
notInSpecs sp' = not . checkInSpecs sp'

-- ListenerSpec makes no sense like this. Surely the HandlerSpec must also
-- depend upon the VerInfo.
data ListenerSpec = ListenerSpec
    { lsHandler :: VerInfo -> Listener -- ^ Handler accepts out verInfo and returns listener
    , lsInSpec  :: (MessageCode, HandlerSpec)
    }

-- | The MessageCode that the listener responds to.
listenerMessageCode :: Listener -> MessageCode
listenerMessageCode (N.Listener (_ :: PeerData -> NodeId -> N.ConversationActions snd rcv -> IO ())) =
    messageCode (Proxy @rcv)

newtype InSpecs = InSpecs HandlerSpecs
  deriving (Eq, Show, Generic)

newtype OutSpecs = OutSpecs HandlerSpecs
  deriving (Eq, Show, Generic)

instance Semigroup InSpecs where
    (InSpecs a) <> (InSpecs b) =
          InSpecs $ HM.unionWithKey merger a b
      where
        merger name h1 h2 =
          error $ sformat
              ("Conflicting key in input spec: "%build%" "%build)
              (name, h1) (name, h2)

instance Monoid InSpecs where
    mempty = InSpecs mempty
    mappend = (<>)

instance Semigroup OutSpecs where
    (OutSpecs a) <> (OutSpecs b) =
          OutSpecs $ HM.unionWithKey merger a b
      where
        merger name h1 h2 =
          if h1 == h2
             then h1
             else error $ sformat
                    ("Conflicting key output spec: "%build%" "%build)
                    (name, h1) (name, h2)

instance Monoid OutSpecs where
    mempty = OutSpecs mempty
    mappend = (<>)

toOutSpecs :: [(MessageCode, HandlerSpec)] -> OutSpecs
toOutSpecs = OutSpecs . merge . fmap (uncurry HM.singleton)
  where
    merge = foldr (HM.unionWithKey merger) mempty
    merger name h1 h2 = error $ sformat
        ("Conflicting key output spec in toOutSpecs: "%build%" at "%build%" "%build)
        name h1 h2

-- | Data type to represent listeners, provided upon our version info and peerData
-- received from other node, in and out specs for all listeners which may be provided
data MkListeners = MkListeners
        { mkListeners :: VerInfo -> PeerData -> [Listener]
        -- ^ Accepts our version info and their peerData and returns set of listeners
        , inSpecs     :: InSpecs
        -- ^ Aggregated specs for what we accept on incoming connections
        , outSpecs    :: OutSpecs
        -- ^ Aggregated specs for which outgoing connections we might initiate
        }

instance Semigroup MkListeners where
    a <> b = MkListeners act (inSpecs a <> inSpecs b) (outSpecs a <> outSpecs b)
      where
        act vI pD = (++) (mkListeners a vI pD) (mkListeners b vI pD)

instance Monoid MkListeners where
    mempty = MkListeners (\_ _ -> []) mempty mempty
    mappend = (<>)

-- | 'Subscribe' message
--
-- Node A can send a MsgSubscribe to Node B to request that Node B adds Node A
-- to the list of known peers in its outbound queue, so that Node B will send
-- messages to node A.
--
-- This is used by behind-NAT nodes, who join the network by sending a
-- MsgSubscribe to a relay nodes.
--
-- Note that after node A subscribes, it doesn't send anything to node B
-- anymore. Therefore it might happen that due to problems within the network
-- (e.g. router failures, see
-- https://blog.stephencleary.com/2009/05/detection-of-half-open-dropped.html
-- for more detailed information) node B closes the subscription channel after
-- it tries to send data to node A and fails, whereas node A will be oblivious
-- to that event and won't try to reestablish the channel. To remedy that node A
-- needs to periodically send keep-alive like data to node B in order to ensure
-- that the connection is valid.
--
-- Kademlia nodes might also use this if they want a guarantee that they receive
-- messages from their peers (without subscription we rely on luck for some
-- nodes to decide to add us to their list of known peers).
data MsgSubscribe = MsgSubscribe | MsgSubscribeKeepAlive
    deriving (Generic, Show, Eq)

-- | Old version of MsgSubscribe.
data MsgSubscribe1 = MsgSubscribe1
    deriving (Generic, Show, Eq)

mlMsgSubscribe :: Limit MsgSubscribe
mlMsgSubscribe = 0

mlMsgSubscribe1 :: Limit MsgSubscribe1
mlMsgSubscribe1 = 0

recvLimited
    :: forall rcv snd .
       N.ConversationActions snd rcv -> Limit rcv -> IO (Maybe rcv)
recvLimited conv = N.recv conv . getLimit
