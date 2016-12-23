{-# LANGUAGE FlexibleInstances #-}

module Pos.Binary.Script () where

import qualified PlutusCore.Program as PLCore
import qualified PlutusCore.Term    as PLCore

import           Pos.Binary.Class   (Bi (..))
import           Pos.Script         (Script)

instance Bi PLCore.Term
instance Bi PLCore.Program    -- Script = PLCore.Program

instance Bi Script
