-- | Types used by web server.

module Pos.Web.Types
       ( SscStage (..)
       , TlsParams (..)
       , CConfirmedProposalState (..)
       ) where

import           Universum

-- | Stages of SSC.
data SscStage
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
