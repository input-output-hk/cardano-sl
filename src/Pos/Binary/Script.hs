{-# LANGUAGE FlexibleInstances #-}

module Pos.Binary.Script () where

import qualified PlutusCore.Program as PLCore
import qualified PlutusCore.Term    as PLCore

import           Pos.Binary.Class   (Bi (..))
import           Pos.Script         ()

instance Bi PLCore.Term where
instance Bi PLCore.Program where    -- Script = PLCore.Program
