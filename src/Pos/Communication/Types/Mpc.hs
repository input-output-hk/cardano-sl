{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Types used for MPC communication.

module Pos.Communication.Types.Mpc
       ( SendSsc (..)
       ) where

import           Data.Binary          (Binary)
import           Universum

import           Control.TimeWarp.Rpc (Message (..))
import           Pos.Crypto           (PublicKey, Share)
import           Pos.Types            (Opening, SignedCommitment, VssCertificate)

-- | Message: some node has sent SscMessage
data SendSsc
    = SendCommitment PublicKey
                     SignedCommitment
    | SendOpening PublicKey
                  Opening
    | SendShares PublicKey
                 (HashMap PublicKey Share)
    | SendVssCertificate PublicKey
                         VssCertificate
    deriving (Show, Generic)

instance Binary SendSsc

instance Message SendSsc where
    messageName _ = "SendSsc"
