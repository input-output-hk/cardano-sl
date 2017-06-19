{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Broadcast.Relay.Class
       ( Relay (..)
       , InvReqDataParams (..)
       , DataParams (..)
       ) where

import           Node                          (NodeId)
import qualified Node.Message.Class            as Msg
import           Universum

import           Network.Broadcast.Relay.Types (ReqMsg, InvOrData, DataMsg,
                                                PropagationMsg)

-- | Data for general Inv/Req/Dat framework

data Relay packingType m where
  InvReqData ::
      ( Buildable contents
      , Buildable key
      , Typeable contents
      , Typeable key
      , Eq key
      , Msg.Serializable packingType (ReqMsg key)
      , Msg.Serializable packingType (InvOrData key contents)
      , Msg.Message (ReqMsg key)
      , Msg.Message (InvOrData key contents)
      )
      => (PropagationMsg packingType -> m ()) -- How to relay the data.
      -> InvReqDataParams key contents m
      -> Relay packingType m
  Data ::
      ( Buildable contents
      , Typeable contents
      , Msg.Serializable packingType (DataMsg contents)
      , Msg.Message (DataMsg contents)
      )
      => (PropagationMsg packingType -> m ()) -- How to relay the data.
      -> DataParams contents m
      -> Relay packingType m

data InvReqDataParams key contents m =
    InvReqDataParams
        {
        -- | Get key for given contents.
          contentsToKey :: contents -> m key

        -- | Handle inv msg and return whether it's useful or not
        , handleInv     :: NodeId -> key -> m Bool

        -- | Handle req msg and return (Just data) in case requested data can be provided
        , handleReq     :: NodeId -> key -> m (Maybe contents)

        -- | Handle data msg and return True if message is to be propagated
        , handleData    :: NodeId -> contents -> m Bool
        }

data DataParams contents m = DataParams
        { handleDataOnly :: NodeId -> contents -> m Bool
        }
