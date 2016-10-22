{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Types used for MPC communication.

module Pos.Communication.Types.Mpc
       ( SendCommitment (..)
       , SendOpening (..)
       , SendShares (..)
       , SendVssCertificate (..)
       ) where

import           Control.TimeWarp.Rpc (mkMessage, mkRequest')
import           Data.Binary          (Binary)
import           Data.MessagePack     (MessagePack)
import           Universum

import           Pos.Crypto           (PublicKey, Share)
import           Pos.Types            (Commitment, CommitmentSignature, Opening,
                                       VssCertificate)

-- | Message: some node (identified by a pubkey) has sent a signed commitment.
data SendCommitment =
    SendCommitment PublicKey (Commitment, CommitmentSignature)
    deriving (Show, Generic)

-- | Message: some node has sent an opening.
data SendOpening =
    SendOpening PublicKey Opening
    deriving (Show, Generic)

-- | Message: some node has sent decrypted shares.
data SendShares =
    SendShares PublicKey (HashMap PublicKey Share)
    deriving (Show, Generic)

-- | Message: some node has sent its VSS certificate.
data SendVssCertificate =
    SendVssCertificate PublicKey VssCertificate
    deriving (Show, Generic)

instance Binary SendCommitment
instance Binary SendOpening
instance Binary SendShares
instance Binary SendVssCertificate

instance MessagePack SendCommitment
instance MessagePack SendOpening
instance MessagePack SendShares
instance MessagePack SendVssCertificate

-- Currently we use 'Void' as the “exception” type, this should be replaced.
mkMessage ''Void

mkRequest' ''SendCommitment ''() ''Void
mkRequest' ''SendOpening ''() ''Void
mkRequest' ''SendShares ''() ''Void
mkRequest' ''SendVssCertificate ''() ''Void
