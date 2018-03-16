-- | Types used by web server.

module Pos.Web.Types
       ( SscStage (..)
       , TlsParams (..)
       , CConfirmedProposalState (..)
       ) where

import           Universum

-- | Stages of SSC.
-- Also called GodTossing algorithm.
-- GodTossing is a coin tossing with guaranteed output delivery.
-- Nodes exchange commitments, openings, and shares, and in the end arrive at a shared seed.
-- See https://eprint.iacr.org/2016/889.pdf (“A Provably Secure Proof-of-Stake Blockchain Protocol”),
-- section 4 for more details.

data SscStage
    = CommitmentStage
    | OpeningStage
    | SharesStage
    | OrdinaryStage

-- | TLS Transport Layer Security file paths.
data TlsParams = TlsParams
    { tpCertPath :: FilePath
    , tpCaPath   :: FilePath
    , tpKeyPath  :: FilePath
    , tpClients  :: [String]
    } deriving (Show)

newtype CConfirmedProposalState = CConfirmedProposalState Text
    deriving (Show, Generic, Buildable)
