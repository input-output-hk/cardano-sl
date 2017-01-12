{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Server which handles update system.

module Pos.Update.Listeners
       ( usListeners
       ) where

import qualified Data.HashMap.Strict     as HM
import           Serokell.Util.Verify    (VerificationRes (..))
import           Universum

import           Pos.Binary.Relay        ()
import           Pos.Communication.Types (MutSocketState, ResponseMode)
import           Pos.DHT.Model           (ListenerDHT (..), MonadDHTDialog)
import           Pos.Update.Types        (ProposalMsgTag (..), UpId, UpdateProposal (..))
import           Pos.Util.Relay          (DataMsg, InvMsg, Relay (..), ReqMsg,
                                          handleDataL, handleInvL, handleReqL)
import           Pos.WorkMode            (WorkMode)

-- | Listeners for requests related to update system
usListeners
    :: (MonadDHTDialog (MutSocketState ssc) m, WorkMode ssc m)
    => [ListenerDHT (MutSocketState ssc) m]
usListeners =
    [ ListenerDHT handleInvProposal
    , ListenerDHT handleReqProposal
    , ListenerDHT handleDataProposal
    ]

handleInvProposal :: ResponseMode ssc m => InvMsg UpId ProposalMsgTag -> m ()
handleInvProposal = handleInvL

handleReqProposal :: ResponseMode ssc m => ReqMsg UpId ProposalMsgTag -> m ()
handleReqProposal = handleReqL

handleDataProposal :: ResponseMode ssc m => DataMsg UpId UpdateProposal -> m ()
handleDataProposal = handleDataL

instance WorkMode ssc m =>
         Relay m ProposalMsgTag UpId UpdateProposal where
    contentsToTag _ = pure ProposalMsgTag

    verifyInvTag _ = pure VerSuccess
    verifyReqTag _ = pure VerSuccess
    -- TODO: maybe somehow check that versions are not decreasing or whatevs?
    verifyDataContents UpdateProposal{..} = pure VerSuccess

    handleInv _ _ = notImplemented
    handleReq _ _ = notImplemented
    handleData _ _ = notImplemented
