{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

-- | Protocol/versioning related communication types.

module Pos.Communication.Types.Protocol
       ( HandlerSpec (..)
       , VerInfo (..)
       , HandlerSpecs
       , checkInSpecs
       , notInSpecs
       , ListenerSpec (..)
       , InSpecs (..)
       , OutSpecs (..)
       , Listener
       , Worker
       , Action
       , SendActions (..)
       , N.ConversationActions (..)
       , Conversation (..)
       , Action'
       , Worker'
       , NSendActions
       , PeerData
       , toOutSpecs
       , convH
       , MkListeners (..)
       , WorkerSpec
       , ActionSpec (..)
       , N.NodeId
       ) where

import qualified Data.HashMap.Strict   as HM
import qualified Data.Text.Buildable   as B
import           Formatting            (bprint, build, hex, int, sformat, stext, (%))
import qualified Node                  as N
import           Node.Message          (Message (..), MessageName (..))
import           Serokell.Util.Base16  (base16F)
import           Serokell.Util.Text    (listJson, mapJson)
import           Universum

import           Pos.Binary.Class      (Bi)
import           Pos.Communication.BiP (BiP)
import           Pos.Core.Types        (BlockVersion)
import           Pos.Util.TimeWarp     (nodeIdToAddress)

type PeerData = VerInfo

type Listener = N.Listener BiP PeerData
type Worker m = Action m ()
type Action m a = NSendActions m -> m a
type Action' m a = SendActions m -> m a
type Worker' m = Action' m ()
type NSendActions = N.SendActions BiP PeerData
newtype ActionSpec m a = ActionSpec (VerInfo -> Action m a)
type WorkerSpec m = ActionSpec m ()

-- TODO move to time-warp-nt
instance Buildable N.NodeId where
    build nNodeId =
        maybe "<unknown host:port>" (uncurry $ bprint (stext%":"%int)) $
                   first decodeUtf8 <$>
                   nodeIdToAddress nNodeId

data SendActions m = SendActions {
       -- | Establish a bi-direction conversation session with a node.
       withConnectionTo
           :: forall t .
              N.NodeId
           -> (PeerData -> NonEmpty (Conversation m t))
           -> m t
}

data Conversation m t where
    Conversation
        :: ( Bi snd, Message snd, Bi rcv, Message rcv )
        => (N.ConversationActions snd rcv m -> m t)
        -> Conversation m t

data HandlerSpec
    = ConvHandler { hsReplyType :: MessageName}
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

data ListenerSpec m = ListenerSpec
    { lsHandler :: VerInfo -> m (Listener m) -- ^ Handler accepts out verInfo and returns listener
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

toOutSpecs :: [(MessageName, HandlerSpec)] -> OutSpecs
toOutSpecs = OutSpecs . HM.fromList

-- | Data type to represent listeners, provided upon our version info and peerData
-- received from other node, in and out specs for all listeners which may be provided
data MkListeners m = MkListeners
        { mkListeners :: VerInfo -> PeerData -> m [Listener m]
        -- ^ Accepts our version info and their peerData and returns set of listeners
        , inSpecs     :: InSpecs
        -- ^ Aggregated specs for what we accept on incoming connections
        , outSpecs    :: OutSpecs
        -- ^ Aggregated specs for which outgoing connections we might initiate
        }

instance Monad m => Monoid (MkListeners m) where
    mempty = MkListeners (\_ _ -> pure []) mempty mempty
    a `mappend` b = MkListeners act (inSpecs a `mappend` inSpecs b) (outSpecs a `mappend` outSpecs b)
      where
        act vI pD = liftM2 (++) (mkListeners a vI pD) (mkListeners b vI pD)
