
module Pos.Recovery.Types
       ( MonadRecoveryHeader
       ) where

import           Universum

import           Pos.Infra.Recovery.Types (RecoveryHeader, RecoveryHeaderTag)
import           Pos.Util.Util (HasLens (..))

type MonadRecoveryHeader ctx m
     = (MonadReader ctx m, HasLens RecoveryHeaderTag ctx RecoveryHeader)
