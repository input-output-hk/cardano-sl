{-# LANGUAGE DeriveLift           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Script.Type
       ( Script(..)
       , Script_v0
       , ScriptVersion
       ) where

import           Data.Binary                (Binary)
import qualified Data.Binary                as Binary
import           Data.Eq.Deriving           (deriveEq1)
import           Data.Hashable              (Hashable, hashWithSalt)
import           Data.SafeCopy              (SafeCopy (..))
import           Data.SafeCopy              (base, deriveSafeCopySimple)
import           Data.Text.Buildable        (Buildable)
import qualified Data.Text.Buildable        as Buildable
import           Formatting                 (bprint, int, (%))
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift (..))
import qualified PlutusCore.Program         as PLCore
import qualified PlutusCore.Term            as PLCore
import qualified PlutusTypes.ConSig         as PLTypes
import qualified PlutusTypes.Type           as PLTypes
import           Universum                  hiding (lift)
import qualified Utils.ABT                  as ABT
import qualified Utils.Names                as Names
import qualified Utils.Vars                 as Vars

import           Pos.Binary.Class           (Bi)
import           Pos.Util                   (getCopyBinary, putCopyBinary)

-- | Version of script
type ScriptVersion = Word16

-- | A script for inclusion into a transaction.
data Script = Script {
    scrVersion :: ScriptVersion,    -- ^ Version
    scrScript  :: LByteString}      -- ^ Serialized script
  deriving (Eq, Show, Generic, Typeable)

instance NFData Script
instance Hashable Script

instance Buildable Script where
    build Script{..} = bprint ("<script v"%int%">") scrVersion

deriveSafeCopySimple 0 'base ''Script

-- | Deserialized script (i.e. an AST), version 0.
type Script_v0 = PLCore.Program

----------------------------------------------------------------------------
-- Orphan instances, to be included into plutus-prototype
----------------------------------------------------------------------------

deriving instance Show PLCore.PrimData

deriving instance (Show a, Show (PLCore.ClauseF a)) => Show (PLCore.TermF a)
deriving instance (Eq a, Eq (PLCore.ClauseF a)) => Eq (PLCore.TermF a)

deriving instance Eq PLTypes.TyConSig
deriving instance Eq PLTypes.ConSig
deriving instance Eq PLTypes.PolymorphicType
deriving instance Eq PLCore.Program

deriveEq1 ''PLCore.PatternF
deriveEq1 ''PLCore.ClauseF
deriveEq1 ''PLCore.TermF
deriveEq1 ''PLTypes.TypeF

deriving instance Show (f PLCore.PatternF) => Show (PLCore.PatternF (f PLCore.PatternF))

deriving instance Show r => Show (PLCore.ClauseF r)

instance Binary Vars.FreeVar
instance Binary Vars.MetaVar
instance Binary Vars.BoundVar
instance Binary PLTypes.TyConSig
instance Binary PLTypes.ConSig
instance Binary PLTypes.PolymorphicType
instance Binary a => Binary (Names.Sourced a)
instance Binary ABT.Variable
instance Binary (f (ABT.Scope f)) => Binary (ABT.ABT f)
instance Binary (f (ABT.Scope f)) => Binary (ABT.Scope f)
instance Binary r => Binary (PLCore.ClauseF r)
instance Binary a => Binary (PLCore.TermF a)
instance Binary a => Binary (PLCore.PatternF a)
instance Binary a => Binary (PLTypes.TypeF a)
instance Binary PLCore.PrimData
instance Binary PLCore.Program

instance NFData Vars.FreeVar
instance NFData Vars.MetaVar
instance NFData Vars.BoundVar
instance NFData PLTypes.TyConSig
instance NFData PLTypes.ConSig
instance NFData PLTypes.PolymorphicType
instance NFData a => NFData (Names.Sourced a)
instance NFData ABT.Variable
instance NFData (f (ABT.Scope f)) => NFData (ABT.ABT f)
instance NFData (f (ABT.Scope f)) => NFData (ABT.Scope f)
instance NFData r => NFData (PLCore.ClauseF r)
instance NFData a => NFData (PLCore.TermF a)
instance NFData a => NFData (PLCore.PatternF a)
instance NFData a => NFData (PLTypes.TypeF a)
instance NFData PLCore.PrimData
instance NFData PLCore.Program

deriving instance Lift Vars.FreeVar
deriving instance Lift Vars.MetaVar
deriving instance Lift Vars.BoundVar
deriving instance Lift PLTypes.TyConSig
deriving instance Lift PLTypes.ConSig
deriving instance Lift PLTypes.PolymorphicType
deriving instance Lift a => Lift (Names.Sourced a)
deriving instance Lift ABT.Variable
deriving instance Lift (f (ABT.Scope f)) => Lift (ABT.ABT f)
deriving instance Lift (f (ABT.Scope f)) => Lift (ABT.Scope f)
deriving instance Lift r => Lift (PLCore.ClauseF r)
deriving instance Lift a => Lift (PLCore.TermF a)
deriving instance Lift a => Lift (PLCore.PatternF a)
deriving instance Lift a => Lift (PLTypes.TypeF a)
deriving instance Lift PLCore.PrimData
deriving instance Lift PLCore.Program

instance Bi PLCore.Term => SafeCopy PLCore.Term where
    getCopy = getCopyBinary "Term"
    putCopy = putCopyBinary

instance Bi PLCore.Program => SafeCopy PLCore.Program where
    getCopy = getCopyBinary "Program"
    putCopy = putCopyBinary

instance Hashable PLCore.Term where
    hashWithSalt s = hashWithSalt s . Binary.encode

instance Hashable PLCore.Program where
    hashWithSalt s = hashWithSalt s . Binary.encode
