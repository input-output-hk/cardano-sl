-- | Types used by web server.

module Pos.Web.Types
       ( GodTossingStage (..)
       ) where

-- | Stages of GodTossing algorithm.
data GodTossingStage
    = CommitmentStage
    | OpeningStage
    | SharesStage
    | OrdinaryStage
