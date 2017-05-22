{-# LANGUAGE RankNTypes #-}

-- | Protocol/versioning related communication types.

module Pos.Communication.Types.Protocol
       ( HandlerSpec (..)
       , VerInfo (..)
       , HandlerSpecs
       , inSpecs
       , notInSpecs
       , ListenerSpec (..)
       , InSpecs (..)
       , OutSpecs (..)
       , PeerId (..)
       , Listener
       , Worker
       , Action
       , NodeId (..)
       , SendActions (..)
       , ConversationActions (..)
       , Action'
       , Worker'
       , NSendActions
       , PeerData
       , mergeLs
       , toOutSpecs
       , oneMsgH
       , convH
       , ListenersWithOut
       , WorkerSpec
       , ActionSpec (..)
       , peerIdParser
       , nodeIdParser
       ) where

import qualified Control.Monad         as Monad (fail)
import           Data.Aeson            (ToJSON (..), FromJSON (..))
import           Data.Hashable         (Hashable)
import qualified Data.HashMap.Strict   as HM
import qualified Data.Text.Buildable   as B
import qualified Data.Text.Encoding    as Text (encodeUtf8, decodeUtf8)
import qualified Data.ByteString       as BS (length)
import qualified Data.ByteString.Base64 as B64 (encode, decode)
import           Formatting            (bprint, build, hex, int, sformat, stext, (%))
import qualified Node                  as N
import           Node.Message          (Message (..), MessageName (..))
import           Serokell.Util.Base16  (base16F)
import           Serokell.Util.Text    (listJson, mapJson)
import           Universum

import           Pos.Binary.Class      (Bi)
import           Pos.Communication.BiP (BiP)
import           Pos.Core.Types        (BlockVersion)
import           Pos.Util.TimeWarp     (addrParser, addressToNodeId, nodeIdToAddress)
import qualified Serokell.Util.Parse   as P
import qualified Text.Parsec           as P
import qualified Text.Parsec.String    as P

type PeerData = (PeerId, VerInfo)

type Listener = N.Listener BiP PeerData
type Worker m = Action m ()
type Action m a = NSendActions m -> m a
type Action' m a = SendActions m -> m a
type Worker' m = Action' m ()
type NSendActions = N.SendActions BiP PeerData
newtype ActionSpec m a = ActionSpec (VerInfo -> Action m a)
type WorkerSpec m = ActionSpec m ()

newtype NodeId = NodeId (PeerId, N.NodeId)
  deriving (Show, Eq, Ord, Hashable)

-- TODO Implement Buildable N.NodeId and get rid of this ugly shit
instance Buildable NodeId where
    build (NodeId (peerId, nNodeId)) =
        let addr = maybe "<unknown host:port>" (uncurry $ sformat (stext%":"%int)) $
                   first decodeUtf8 <$>
                   nodeIdToAddress nNodeId
        in bprint (stext%"/"%build) addr peerId

data SendActions m = SendActions {
       -- | Send a isolated (sessionless) message to a node
       sendTo :: forall msg .
              ( Bi msg, Message msg )
              => NodeId
              -> msg
              -> m (),

       -- | Establish a bi-direction conversation session with a node.
       withConnectionTo
           :: forall snd rcv t .
            ( Bi snd, Message snd, Bi rcv, Message rcv )
           => NodeId
           -> (m PeerData -> ConversationActions snd rcv m -> m t)
           -> m t
}

data ConversationActions body rcv m = ConversationActions {
       -- | Send a message within the context of this conversation
       send :: body -> m ()

       -- | Receive a message within the context of this conversation.
       --   'Nothing' means end of input (peer ended conversation).
     , recv :: m (Maybe rcv)
}

newtype PeerId = PeerId ByteString
  deriving (Eq, Ord, Show, Generic, Hashable)

instance ToJSON PeerId where
    toJSON (PeerId bs) = toJSON (Text.decodeUtf8 (B64.encode bs))

instance FromJSON PeerId where
    parseJSON v = do
        bs <- Text.encodeUtf8 <$> parseJSON v
        case B64.decode bs of
            Left err -> fail err
            Right decoded -> pure $ PeerId decoded

instance Buildable PeerId where
    build (PeerId bs) = bprint base16F bs

data HandlerSpec
    = ConvHandler { hsReplyType :: MessageName}
    | OneMsgHandler
    | UnknownHandler Word8 ByteString
    deriving (Show, Generic, Eq)

convH :: (Message snd, Message rcv) => Proxy snd -> Proxy rcv -> (MessageName, HandlerSpec)
convH pSnd pReply = (messageName pSnd, ConvHandler $ messageName pReply)

oneMsgH :: Message snd => Proxy snd -> (MessageName, HandlerSpec)
oneMsgH pSnd = (messageName pSnd, OneMsgHandler)

instance Buildable HandlerSpec where
    build OneMsgHandler =
        "OneMsg"
    build (ConvHandler (MessageName replyType)) =
        bprint ("Conv "%base16F) replyType
    build (UnknownHandler htype hcontent) =
        bprint ("UnknownHandler "%hex%" "%base16F) htype hcontent

instance Buildable (MessageName, HandlerSpec) where
    build (MessageName rcvType, h) = bprint (base16F % " -> " % build) rcvType h

type HandlerSpecs = HashMap MessageName HandlerSpec

instance Buildable HandlerSpecs where
    build x = bprint ("HandlerSpecs: "%listJson) (HM.toList x)

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

inSpecs :: (MessageName, HandlerSpec) -> HandlerSpecs -> Bool
inSpecs (name, sp) specs = case name `HM.lookup` specs of
                              Just sp' -> sp == sp'
                              _        -> False

notInSpecs :: (MessageName, HandlerSpec) -> HandlerSpecs -> Bool
notInSpecs sp' = not . inSpecs sp'

data ListenerSpec m = ListenerSpec
    { lsHandler :: VerInfo -> Listener m -- ^ Handler accepts out verInfo and returns listener
    , lsInSpec  :: (MessageName, HandlerSpec)
    }

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

mergeLs :: [(ListenerSpec m, OutSpecs)] -> ([ListenerSpec m], OutSpecs)
mergeLs = second mconcat . unzip

toOutSpecs :: [(MessageName, HandlerSpec)] -> OutSpecs
toOutSpecs = OutSpecs . HM.fromList

type ListenersWithOut m = ([ListenerSpec m], OutSpecs)

----------
-- Parsers
----------

-- | Parser for PeerId. Any base64 string.
peerIdParser :: P.Parser PeerId
peerIdParser = do
    bytes <- P.base64Url
    when (BS.length bytes /= 14) $ Monad.fail "PeerId must be exactly 14 bytes"
    return $ PeerId bytes

-- | Parser for NodeId
--   host:port/peerId
nodeIdParser :: P.Parser NodeId
nodeIdParser = do
    addr <- addrParser
    _ <- P.char '/'
    peerId <- peerIdParser
    return $ NodeId (peerId, addressToNodeId addr)
