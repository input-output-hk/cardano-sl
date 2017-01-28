{-# LANGUAGE RankNTypes #-}

-- | Protocol/versioning related communication types.

module Pos.Communication.Types.Protocol
       ( HandlerSpec (..)
       , VerInfo (..)
       , HandlerSpecs
       , inSpecs
       , notInSpecs
       , WorkerSpecs (..)
       , ListenerSpec (..)
       , ListenerSpecs (..)
       , toLss
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
       ) where

import           Data.Hashable         (Hashable)
import qualified Data.HashMap.Strict   as HM
import qualified Data.Text.Buildable   as B
import           Formatting            (bprint, build, sformat, shown, (%))
import qualified Node                  as N
import           Node.Message          (Message, MessageName (..))
import           Serokell.Util.Base16  (base16F)
import           Universum

import           Pos.Binary.Class      (Bi)
import           Pos.Communication.BiP (BiP)
import           Pos.Types.Core        (BlockVersion)
import           Pos.Util.TimeWarp     (nodeIdToAddress)

type Listener = N.Listener BiP PeerId
type Worker m = Action m ()
type Action m a = NSendActions m -> m a
type Action' m a = SendActions m -> m a
type Worker' m = Action' m ()
type NSendActions = N.SendActions BiP PeerId

newtype NodeId = NodeId (PeerId, N.NodeId)
  deriving (Show, Eq, Ord, Hashable)

-- TODO Implement Buildable N.NodeId and get rid of this ugly shit
instance Buildable NodeId where
    build (NodeId (peerId, nNodeId)) =
        bprint (shown%"/"%build) (nodeIdToAddress nNodeId) peerId

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
           -> (ConversationActions snd rcv m -> m t)
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

instance Buildable PeerId where
    build (PeerId bs) = bprint base16F bs

data HandlerSpec
  = ConvHandler { hsReplyType :: MessageName }
  | OneMsgHandler
    deriving (Show, Generic, Eq)

instance Buildable HandlerSpec where
    build OneMsgHandler                         = "OneMsg"
    build (ConvHandler (MessageName replyType)) = bprint ("Conv "%base16F) replyType

instance Buildable (MessageName, HandlerSpec) where
    build (MessageName rcvType, h) = bprint (base16F % " -> " % build) rcvType h

type HandlerSpecs = HashMap MessageName HandlerSpec

data VerInfo = VerInfo
    { vIMagic        :: Int32
    , vIBlockVersion :: BlockVersion
    , vIInHandlers   :: HandlerSpecs
    , vIOutHandlers  :: HandlerSpecs
    } deriving (Generic)

inSpecs :: (MessageName, HandlerSpec) -> HandlerSpecs -> Bool
inSpecs (name, sp) specs = case name `HM.lookup` specs of
                              Just sp' -> sp == sp'
                              _        -> False

notInSpecs :: (MessageName, HandlerSpec) -> HandlerSpecs -> Bool
notInSpecs sp' = not . inSpecs sp'

data ListenerSpec m = ListenerSpec
    { lsHandler  :: VerInfo -> Listener m
    , lsInSpec   :: (MessageName, HandlerSpec)
    , lsOutSpecs :: HandlerSpecs
    }

data ListenerSpecs m = ListenerSpecs
    { lssHandlers :: [VerInfo -> Listener m]
    , lssInSpecs  :: HandlerSpecs
    , lssOutSpecs :: HandlerSpecs
    }

lssSingleton :: ListenerSpec m -> ListenerSpecs m
lssSingleton ListenerSpec {..} = ListenerSpecs
    { lssHandlers = [lsHandler]
    , lssInSpecs = HM.fromList [lsInSpec]
    , lssOutSpecs = lsOutSpecs
    }

toLss :: [ListenerSpec m] -> ListenerSpecs m
toLss = mconcat . map lssSingleton

hssOutUnion :: HandlerSpecs -> HandlerSpecs -> HandlerSpecs
hssOutUnion a b = HM.unionWithKey merger a b
  where
    merger name h1 h2 =
      if h1 == h2
         then h1
         else panic $ sformat
                ("Conflicting key output spec: "%build%" "%build)
                (name, h1) (name, h2)

hssInUnion :: HandlerSpecs -> HandlerSpecs -> HandlerSpecs
hssInUnion a b = HM.unionWithKey merger a b
  where
    merger name h1 h2 =
      panic $ sformat
          ("Conflicting key in input spec: "%build%" "%build)
          (name, h1) (name, h2)

instance Monoid (ListenerSpecs m) where
    mempty = ListenerSpecs mempty mempty mempty
    a `mappend` b = ListenerSpecs
        { lssHandlers = lssHandlers a ++ lssHandlers b
        , lssInSpecs = hssInUnion (lssInSpecs a) (lssInSpecs b)
        , lssOutSpecs = hssOutUnion (lssOutSpecs a) (lssOutSpecs b)
        }

data WorkerSpecs m = WorkerSpecs
    { wssExecutors :: [VerInfo -> Worker m]
    , wssOutSpecs  :: HandlerSpecs
    }

instance Monoid (WorkerSpecs m) where
    mempty = WorkerSpecs mempty mempty
    a `mappend` b = WorkerSpecs
        { wssExecutors = wssExecutors a ++ wssExecutors b
        , wssOutSpecs = hssOutUnion (wssOutSpecs a) (wssOutSpecs b)
        }
