-- | Protocol/versioning related communication types.

module Pos.Communication.Types.Protocol
       ( HandlerSpec (..)
       , VerInfo (..)
       , HandlerSpecs
       , inSpecs
       , notInSpecs
       , WorkerSpec (..)
       , ListenerSpec (..)
       ) where

import qualified Data.HashMap.Strict   as HM
import qualified Data.Text.Buildable   as B
import           Formatting            (bprint, build, (%))
import           Node                  (Listener, Worker)
import           Node.Message          (MessageName (..))
import           Serokell.Util.Base16  (base16F)
import           Universum

import           Pos.Communication.BiP (BiP)
import           Pos.Types             (BlockVersion)

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

data WorkerSpec d m = WorkerSpec
    { wsExecutor :: VerInfo -> Worker BiP d m
    , wsOutSpecs :: HandlerSpecs
    }

data ListenerSpec d m = ListenerSpec
    { lsHandler  :: VerInfo -> Listener BiP d m
    , lsInSpec   :: (MessageName, HandlerSpec)
    , lsOutSpecs :: HandlerSpecs
    }
