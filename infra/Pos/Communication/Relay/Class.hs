{-# LANGUAGE TypeFamilies #-}

module Pos.Communication.Relay.Class
       ( Relay (..)
       , MonadRelayMem (..)
       ) where

import           Control.Monad.Trans           (MonadTrans)
import           Node.Message                  (Message)
import           Serokell.Util.Verify          (VerificationRes)
import           Universum

import           Pos.Communication.Limits      (MessageLimited)
import           Pos.Communication.Relay.Types (RelayContext)
import           Pos.Communication.Types.Relay (DataMsg, InvOrData, ReqMsg (..))

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
    verifyDataContents :: contents -> m VerificationRes

    -- | Handle inv msg and return whether it's useful or not
    handleInv :: tag -> key -> m Bool

    -- | Handle req msg and return (Just data) in case requested data can be provided
    handleReq :: tag -> key -> m (Maybe contents)

    -- | Handle data msg and return True if message is to be propagated
    handleData :: contents -> m Bool

class Monad m => MonadRelayMem m where
    askRelayMem :: m RelayContext

    default askRelayMem :: (MonadTrans t, MonadRelayMem m', t m' ~ m) =>
       m RelayContext
    askRelayMem = lift askRelayMem

instance MonadRelayMem m => MonadRelayMem (ReaderT s m) where
instance MonadRelayMem m => MonadRelayMem (ExceptT s m) where
instance MonadRelayMem m => MonadRelayMem (StateT s m) where
