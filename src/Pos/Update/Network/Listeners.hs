{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Server which handles update system.

module Pos.Update.Network.Listeners
       ( usListeners
       ) where

import           Node                     (ListenerAction (..))
import           Serokell.Util.Verify     (VerificationRes (..))
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Binary.Relay         ()
import           Pos.Communication.BiP    (BiP)
import           Pos.Update.Core          (UpId, UpdateProposal (..))
import           Pos.Update.Network.Types (ProposalMsgTag (..))
import           Pos.Util.Relay           (DataMsg, InvMsg, Relay (..), ReqMsg,
                                           handleDataL, handleInvL, handleReqL)
import           Pos.WorkMode             (WorkMode)

-- | Listeners for requests related to update system
usListeners
    :: (WorkMode ssc m)
    => [ListenerAction BiP m]
usListeners =
    [ handleInvProposal
    , handleReqProposal
    , handleDataProposal
    ]

handleInvProposal :: WorkMode ssc m => ListenerAction BiP m
handleInvProposal = ListenerActionOneMsg $ \peerId sendActions (i :: InvMsg UpId ProposalMsgTag) ->
    handleInvL i peerId sendActions

handleReqProposal :: WorkMode ssc m => ListenerAction BiP m
handleReqProposal = ListenerActionOneMsg $ \peerId sendActions (i :: ReqMsg UpId ProposalMsgTag) ->
    handleReqL i peerId sendActions

handleDataProposal :: WorkMode ssc m => ListenerAction BiP m
handleDataProposal = ListenerActionOneMsg $ \peerId sendActions (i :: DataMsg UpId UpdateProposal) ->
    handleDataL i peerId sendActions

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
