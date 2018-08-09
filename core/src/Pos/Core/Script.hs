{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Core.Script () where

import           Universum

import           Data.Eq.Deriving (deriveEq1)
import qualified PlutusCore.Program as PLCore
import qualified PlutusCore.Term as PLCore
import qualified PlutusTypes.ConSig as PLTypes
import qualified PlutusTypes.Type as PLTypes
import qualified Utils.ABT as ABT
import qualified Utils.Names as Names
import qualified Utils.Vars as Vars

----------------------------------------------------------------------------
-- Orphan instances, to be included into plutus-prototype
----------------------------------------------------------------------------

deriving instance Show PLCore.PrimData
deriving instance Show PLCore.SimplePattern

deriving instance (Show a, Show (PLCore.ClauseF a)) => Show (PLCore.TermF a)
deriving instance (Eq a, Eq (PLCore.ClauseF a)) => Eq (PLCore.TermF a)

deriving instance Eq PLTypes.TyConSig
deriving instance Eq PLTypes.ConSig
deriving instance Eq PLCore.SimplePattern
deriving instance Eq PLCore.Program

deriveEq1 ''PLCore.ClauseF
deriveEq1 ''PLCore.TermF
deriveEq1 ''PLTypes.TypeF

deriving instance Show r => Show (PLCore.ClauseF r)

instance NFData Vars.FreeVar
instance NFData Vars.MetaVar
instance NFData Vars.BoundVar
instance NFData PLTypes.TyConSig
instance NFData PLTypes.ConSig
instance NFData a => NFData (Names.Sourced a)
instance NFData ABT.Variable
instance NFData (f (ABT.Scope f)) => NFData (ABT.ABT f)
instance NFData (f (ABT.Scope f)) => NFData (ABT.Scope f)
instance NFData r => NFData (PLCore.ClauseF r)
instance NFData a => NFData (PLCore.TermF a)
instance NFData a => NFData (PLTypes.TypeF a)
instance NFData PLCore.PrimData
instance NFData PLCore.SimplePattern
instance NFData PLCore.Program
