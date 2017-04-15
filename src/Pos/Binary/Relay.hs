{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Pos.Communication.Relay serialization instances

module Pos.Binary.Relay () where

import           Universum

import           Pos.Binary.Class                 (Bi (..), getWord8, label, putWord8)
import           Pos.Binary.Crypto                ()
import           Pos.Binary.Ssc                   ()
import           Pos.Binary.Update                ()
import           Pos.Communication.Types.Relay    (DataMsg (..), InvMsg (..),
                                                   MempoolMsg (..), ReqMsg (..))
import           Pos.Crypto                       (hash)
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents (..))
import           Pos.Txp.Network.Types            (TxMsgContents (..))
import           Pos.Update.Core                  (UpdateProposal, UpdateVote (..))

instance (Bi tag, Bi key) => Bi (InvMsg key tag) where
    put InvMsg {..} = put imTag >> put imKey
    get = label "InvMsg" $ liftM2 InvMsg get get

instance (Bi tag, Bi key) => Bi (ReqMsg key tag) where
    put ReqMsg {..} = put rmTag >> put rmKey
    get = label "ReqMsg" $ liftM2 ReqMsg get get

instance (Bi tag) => Bi (MempoolMsg tag) where
    -- The extra byte is needed because time-warp doesn't work with
    -- possibly-empty messages. 228 was chosen as homage to @pva701
    put MempoolMsg {..} = putWord8 228 >> put mmTag
    get = label "MempoolMsg" $ do
        x <- getWord8
        when (x /= 228) $ fail "wrong byte"
        MempoolMsg <$> get

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
