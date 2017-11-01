-- | Types used by web server.

module Pos.Web.Types
       ( GodTossingStage (..)
       , TlsParams (..)
       , CConfirmedProposalState (..)
       ) where

import           Universum

-- | Stages of GodTossing algorithm.
data GodTossingStage
    = CommitmentStage
    | OpeningStage
    | SharesStage
    | OrdinaryStage

data TlsParams = TlsParams
    { tpCertPath :: FilePath
    , tpCaPath   :: FilePath
    , tpKeyPath  :: FilePath
    } deriving (Show)

newtype CConfirmedProposalState = CConfirmedProposalState Text
    deriving (Show, Generic, Buildable)
