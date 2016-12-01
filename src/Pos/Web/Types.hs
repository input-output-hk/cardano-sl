{-# LANGUAGE TemplateHaskell #-}

-- | Types used by web server.

module Pos.Web.Types
       ( GodTossingStage (..)
       ) where

import           Data.Aeson.TH          (deriveToJSON)
import           Serokell.Aeson.Options (defaultOptions)

-- | Stages of GodTossing algorithm.
data GodTossingStage
    = CommitmentStage
    | OpeningStage
    | SharesStage
    | OrdinaryStage

deriveToJSON defaultOptions ''GodTossingStage
