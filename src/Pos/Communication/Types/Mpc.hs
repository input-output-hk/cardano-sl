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

import           Data.Binary          (Binary)
import           Data.MessagePack     (MessagePack)
import           Universum

import           Control.TimeWarp.Rpc (Message (..))
import           Pos.Crypto           (PublicKey, Share)
import           Pos.Types            (Opening, SignedCommitment, VssCertificate)

-- | Message: some node (identified by a pubkey) has sent a signed commitment.
data SendCommitment =
    SendCommitment PublicKey SignedCommitment
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

instance Message SendCommitment where
    messageName _ = "SendCommitment"

instance Message SendOpening where
    messageName _ = "SendOpening"

instance Message SendShares where
    messageName _ = "SendShares"

instance Message SendVssCertificate where
    messageName _ = "SendVssCertificate"

{-
mkRequest' ''SendCommitment ''() ''Void
mkRequest' ''SendOpening ''() ''Void
mkRequest' ''SendShares ''() ''Void
mkRequest' ''SendVssCertificate ''() ''Void
-}
