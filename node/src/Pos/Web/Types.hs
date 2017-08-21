-- | Types used by web server.

module Pos.Web.Types
       ( GodTossingStage (..)
       , TlsParams (..)
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
