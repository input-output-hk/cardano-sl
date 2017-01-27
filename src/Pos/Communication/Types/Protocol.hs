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
       ) where

import           Data.Hashable         (Hashable)
import qualified Data.HashMap.Strict   as HM
import qualified Data.Text.Buildable   as B
import           Formatting            (bprint, build, sformat, (%))
import           Node                  (Listener, Worker)
import           Node.Message          (MessageName (..))
import           Serokell.Util.Base16  (base16F)
import           Universum

import           Pos.Communication.BiP (BiP)
import           Pos.Types             (BlockVersion)

newtype PeerId = PeerId ByteString
  deriving (Eq, Ord, Show, Generic, Hashable)

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
    } deriving (Show, Generic)

inSpecs :: (MessageName, HandlerSpec) -> HandlerSpecs -> Bool
inSpecs (name, sp) specs = case name `HM.lookup` specs of
                              Just sp' -> sp == sp'
                              _        -> False

notInSpecs :: (MessageName, HandlerSpec) -> HandlerSpecs -> Bool
notInSpecs sp' = not . inSpecs sp'

data ListenerSpec d m = ListenerSpec
    { lsHandler  :: VerInfo -> Listener BiP d m
    , lsInSpec   :: (MessageName, HandlerSpec)
    , lsOutSpecs :: HandlerSpecs
    }

data ListenerSpecs d m = ListenerSpecs
    { lssHandlers :: [VerInfo -> Listener BiP d m]
    , lssInSpecs  :: HandlerSpecs
    , lssOutSpecs :: HandlerSpecs
    }

lssSingleton :: ListenerSpec d m -> ListenerSpecs d m
lssSingleton ListenerSpec {..} = ListenerSpecs
    { lssHandlers = [lsHandler]
    , lssInSpecs = HM.fromList [lsInSpec]
    , lssOutSpecs = lsOutSpecs
    }

toLss :: [ListenerSpec d m] -> ListenerSpecs d m
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

instance Monoid (ListenerSpecs d m) where
    mempty = ListenerSpecs mempty mempty mempty
    a `mappend` b = ListenerSpecs
        { lssHandlers = lssHandlers a ++ lssHandlers b
        , lssInSpecs = hssInUnion (lssInSpecs a) (lssInSpecs b)
        , lssOutSpecs = hssOutUnion (lssOutSpecs a) (lssOutSpecs b)
        }

data WorkerSpecs d m = WorkerSpecs
    { wssExecutors :: [VerInfo -> Worker BiP d m]
    , wssOutSpecs  :: HandlerSpecs
    }

instance Monoid (WorkerSpecs d m) where
    mempty = WorkerSpecs mempty mempty
    a `mappend` b = WorkerSpecs
        { wssExecutors = wssExecutors a ++ wssExecutors b
        , wssOutSpecs = hssOutUnion (wssOutSpecs a) (wssOutSpecs b)
        }
