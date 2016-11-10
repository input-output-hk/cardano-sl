{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Types used for MPC communication.

module Pos.Communication.Types.Mpc
       ( SendSsc (..)
       ) where

import           Data.Binary          (Binary)
import           Data.List.NonEmpty   (NonEmpty)
import           Universum

import           Control.TimeWarp.Rpc (Message (..))
import           Pos.Crypto           (PublicKey, Share)
import           Pos.Ssc.DynamicState (Opening, SignedCommitment, VssCertificate)

-- | Message: some node has sent SscMessage
data SendSsc
    = SendCommitments (NonEmpty (PublicKey, SignedCommitment))
    | SendOpenings (NonEmpty (PublicKey, Opening))
    | SendSharesMulti (NonEmpty (PublicKey, (HashMap PublicKey Share)))
    | SendVssCertificates (NonEmpty (PublicKey, VssCertificate))
    deriving (Show, Generic)

instance Binary SendSsc

instance Message SendSsc where
    messageName _ = "SendSsc"
