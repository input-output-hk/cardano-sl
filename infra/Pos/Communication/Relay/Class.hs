{-# LANGUAGE TypeFamilies #-}

module Pos.Communication.Relay.Class
       ( Relay (..)
       , MonadRelayMem
       , askRelayMem
       ) where

import qualified Control.Monad.Ether.Implicit     as Ether
import           Node.Message                     (Message)
import           Serokell.Util.Verify             (VerificationRes)
import           Universum

import           Pos.Communication.Limits.Types   (MessageLimited)
import           Pos.Communication.Types.Protocol (NodeId)
import           Pos.Communication.Relay.Types    (RelayContext)
import           Pos.Communication.Types.Relay    (DataMsg, InvOrData, MempoolMsg,
                                                   ReqMsg (..))

-- | Typeclass for general Inv/Req/Dat framework. It describes monads,
-- that store data described by tag, where "key" stands for node
-- identifier.
class ( Buildable tag
      , Buildable contents
      , Buildable key
      , Typeable tag
      , Typeable contents
      , Typeable key
      , Message (ReqMsg key tag)
      , Message (MempoolMsg tag)
      , Message (InvOrData tag key contents)
      , MessageLimited (DataMsg contents)
      ) => Relay m tag key contents
      | tag -> contents, contents -> tag, contents -> key, tag -> key where
    -- | Converts data to tag. Tag returned in monad `m`
    -- for only type matching reason (multiparam type classes are tricky)
    contentsToTag :: contents -> m tag

    -- | Same for key. Sometime contents has key inside already, so
    -- it's redundant to double-pass it everywhere.
    contentsToKey :: contents -> m key

    verifyInvTag :: tag -> m VerificationRes
    verifyReqTag :: tag -> m VerificationRes
    verifyMempoolTag :: tag -> m VerificationRes
    verifyDataContents :: contents -> m VerificationRes

    -- | Handle inv msg and return whether it's useful or not
    handleInv :: NodeId -> tag -> key -> m Bool

    -- | Handle req msg and return (Just data) in case requested data can be provided
    handleReq :: NodeId -> tag -> key -> m (Maybe contents)

    -- | Handle mempool msg and return all keys we want to send
    handleMempool :: NodeId -> tag -> m [key]

    -- | Handle data msg and return True if message is to be propagated
    handleData :: NodeId -> contents -> m Bool

type MonadRelayMem = Ether.MonadReader RelayContext

askRelayMem :: MonadRelayMem m => m RelayContext
askRelayMem = Ether.ask
