{-# LANGUAGE BangPatterns #-}

-- | Pos.Communication.Relay serialization instances

module Pos.Binary.Relay () where

import           Data.Binary.Get                  (label)
import           Universum

import           Pos.Binary.Class                 (Bi (..))
import           Pos.Binary.Crypto                ()
import           Pos.Binary.Ssc                   ()
import           Pos.Communication.Types.Relay    (DataMsg (..), InvMsg (..), ReqMsg (..))
import           Pos.Crypto                       (hash)
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents (..))
import           Pos.Txp.Types.Communication      (TxMsgContents (..))
import           Pos.Update.Core                  (UpdateProposal, UpdateVote (..))

instance (Bi tag, Bi key) => Bi (InvMsg key tag) where
    put InvMsg {..} = put imTag >> put imKeys
    get = label "InvMsg" $ liftM2 InvMsg get get

instance (Bi tag, Bi key) => Bi (ReqMsg key tag) where
    put ReqMsg {..} = put rmTag >> put rmKeys
    get = label "ReqMsg" $ liftM2 ReqMsg get get

instance Bi (DataMsg GtMsgContents) where
    put (DataMsg dmContents) = put dmContents
    get = label "DataMsg GtMsgContents" $ DataMsg <$> get

instance Bi (DataMsg TxMsgContents) where
    put (DataMsg (TxMsgContents dmTx dmWitness dmDistr)) =
        put dmTx >> put dmWitness >> put dmDistr
    get = label "DataMsg TxMsgContents" $
        DataMsg <$> (TxMsgContents <$> get <*> get <*> get)

instance Bi (DataMsg (UpdateProposal, [UpdateVote])) where
    put (DataMsg dmContents) = put dmContents
    get = label "DataMsg (UpdateProposal, [UpdateVote])" $ do
        c@(up, votes) <- get
        let !id = hash up
        unless (all ((id ==) . uvProposalId) votes) $
            fail "get@DataMsg@Update: vote's uvProposalId must be equal UpId"
        pure $ DataMsg c

instance Bi (DataMsg UpdateVote) where
    put (DataMsg uv) = put uv
    get = label "DataMsg UpdateVote" $ DataMsg <$> get
