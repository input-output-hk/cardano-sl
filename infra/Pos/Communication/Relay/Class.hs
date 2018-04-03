{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Pos.Communication.Relay.Class
       ( Relay (..)
       , InvReqDataParams (..)
       , DataParams (..)
       , MempoolParams (..)
       ) where

import           Node.Message.Class (Message)
import           Pos.Binary.Class (Bi)
import           Universum

import           Pos.Communication.Limits.Types (Limit)
import           Pos.Communication.Types.Protocol (EnqueueMsg, Msg, NodeId)
import           Pos.Communication.Types.Relay (DataMsg, InvMsg, InvOrData, MempoolMsg, ReqMsg,
                                                ReqOrRes)
import           Pos.Network.Types (Origin)

-- | Data for general Inv/Req/Dat framework

data Relay where
  InvReqData ::
      ( Buildable contents
      , Buildable key
      , Typeable contents
      , Typeable key
      , Eq key
      , Bi (ReqMsg key)
      , Bi (ReqOrRes key)
      , Bi (InvOrData key contents)
      , Message (ReqMsg key)
      , Message (ReqOrRes key)
      , Message (InvOrData key contents)
      ) => MempoolParams -> InvReqDataParams key contents -> Relay
  Data ::
      ( Buildable contents
      , Typeable contents
      , Bi (DataMsg contents)
      , Message (DataMsg contents)
      ) => DataParams contents -> Relay

data MempoolParams where
    NoMempool :: MempoolParams
    -- `tag` is used only as type param, no actual param used
    KeyMempool ::
      ( Message (MempoolMsg tag)
      , Message (InvMsg key)
      , Bi (MempoolMsg tag)
      , Bi (InvMsg key)
      , Buildable key
      , Typeable tag
      , Typeable key
      ) => Proxy tag -> IO [key] -> MempoolParams

data InvReqDataParams key contents = InvReqDataParams
    { invReqMsgType :: !(Origin NodeId -> Msg)
    , contentsToKey :: contents -> IO key
      -- ^ Get key for given contents.
    , handleInv     :: NodeId -> key -> IO Bool
      -- ^ Handle inv msg and return whether it's useful or not
    , handleReq     :: NodeId -> key -> IO (Maybe contents)
      -- ^ Handle req msg and return (Just data) in case requested data can be provided
    , handleData    :: NodeId -> contents -> IO Bool
      -- ^ Handle data msg and return True if message is to be propagated
    , irdpMkLimit   :: IO (Limit contents)
    }

data DataParams contents = DataParams
    { dataMsgType    :: !(Origin NodeId -> Msg)
    , handleDataOnly :: EnqueueMsg -> NodeId -> contents -> IO Bool
      -- ^ Handle data msg and return True if message is to be propagated
    , dpMkLimit      :: IO (Limit contents)
    }
