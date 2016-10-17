{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Types used for MPC communication.

module Pos.Communication.Types.Mpc
       ( SendOpening(..)
       , SendCommitment(..)
       ) where

import           Control.TimeWarp.Rpc (mkMessage, mkRequest')
import           Data.Binary          (Binary)
import           Data.MessagePack     (MessagePack)
import           Universum

import           Pos.Crypto           (PublicKey)
import           Pos.Types            (Commitment, CommitmentSignature, Opening)

-- | Message: some node (identified by a pubkey) has sent an opening.
data SendOpening =
    SendOpening PublicKey Opening
    deriving (Show, Generic)

-- | Message: some node (identified by a pubkey) has sent a signed commitment.
data SendCommitment =
    SendCommitment PublicKey (Commitment, CommitmentSignature)
    deriving (Show, Generic)

instance Binary SendOpening
instance Binary SendCommitment

instance MessagePack SendOpening
instance MessagePack SendCommitment

-- Currently we use 'Void' as the “exception” type, this should be replaced.
mkMessage ''Void

mkRequest' ''SendOpening ''() ''Void
mkRequest' ''SendCommitment ''() ''Void
