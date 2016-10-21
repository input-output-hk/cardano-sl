{-# LANGUAGE ConstraintKinds #-}

-- | Types used for communication.

module Pos.Communication.Types
       ( ResponseMode

       -- * Request types
       , SendOpening (..)
       , SendCommitment (..)
       , module Block
       ) where

import           Control.TimeWarp.Rpc          (MonadDialog, MonadResponse)
-- import           Universum

import           Pos.Communication.Types.Block as Block
import           Pos.Communication.Types.Mpc   (SendCommitment (..), SendOpening (..))
import           Pos.WorkMode                  (WorkMode)

-- TODO: MonadDialog should be part of WorkMode
type ResponseMode m = (WorkMode m, MonadResponse m, MonadDialog m)
