{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Communication.Relay.Class
       ( Relay (..)
       , InvReqDataParams (..)
       , DataParams (..)
       , MempoolParams (..)
       , MonadRelayMem
       , askRelayMem
       ) where

import qualified Ether
import           Node.Message                   (Message)
import           Pos.Binary.Class               (Bi)
import           Universum

import           Pos.Communication.Limits.Types (MessageLimited)
import           Pos.Communication.Relay.Types  (RelayContext)
import           Pos.Communication.Types.Relay  (DataMsg, InvMsg, InvOrData, MempoolMsg,
                                                 ReqMsg (..))

-- | Data for general Inv/Req/Dat framework

data Relay m where
  InvReqData ::
      ( Buildable contents
      , Buildable key
      , Typeable contents
      , Typeable key
      , Eq key
      , Bi (ReqMsg key)
      , Bi (InvOrData key contents)
      , Message (ReqMsg key)
      , Message (InvOrData key contents)
      , MessageLimited (DataMsg contents)
      ) => MempoolParams m -> InvReqDataParams key contents m -> Relay m
  Data ::
      ( Buildable contents
      , Typeable contents
      , Bi (DataMsg contents)
      , Message (DataMsg contents)
      , MessageLimited (DataMsg contents)
      ) => DataParams contents m -> Relay m

data MempoolParams m where
    NoMempool :: MempoolParams m
    -- `tag` is used only as type param, no actual param used
    KeyMempool ::
      ( Message (MempoolMsg tag)
      , Message (InvMsg key)
      , Bi (MempoolMsg tag)
      , Bi (InvMsg key)
      , Buildable key
      , Typeable tag
      , Typeable key
      ) => Proxy tag -> m [key] -> MempoolParams m

data InvReqDataParams key contents m =
    InvReqDataParams
        {
        -- | Get key for given contents.
          contentsToKey :: contents -> m key

        -- | Handle inv msg and return whether it's useful or not
        , handleInv     :: key -> m Bool

        -- | Handle req msg and return (Just data) in case requested data can be provided
        , handleReq     :: key -> m (Maybe contents)

        -- | Handle data msg and return True if message is to be propagated
        , handleData    :: contents -> m Bool
        }

data DataParams contents m = DataParams
        { handleDataOnly :: contents -> m Bool
        }


type MonadRelayMem = Ether.MonadReader' RelayContext

askRelayMem :: MonadRelayMem m => m RelayContext
askRelayMem = Ether.ask'
