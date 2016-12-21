{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A wrapper over Plutus (the scripting language used in transactions).
module Pos.Script
       ( Script
       , TxScriptError

       , txScriptCheck

       , parseValidator
       , parseRedeemer
       ) where

import           Data.Binary           (Binary)
import qualified Data.Binary           as Binary
import           Data.Eq.Deriving      (deriveEq1)
import           Data.Hashable         (Hashable, hashWithSalt)
import           Data.SafeCopy         (SafeCopy (..))
import           Data.String           (String)
import qualified Interface.Integration as PL
import qualified PlutusCore.Program    as PLCore
import qualified PlutusCore.Term       as PLCore
import qualified PlutusTypes.ConSig    as PLTypes
import qualified PlutusTypes.Type      as PLTypes
import           Universum
import qualified Utils.ABT             as ABT
import qualified Utils.Names           as Names
import qualified Utils.Vars            as Vars

import           Pos.Binary.Class      (Bi)
import           Pos.Util              (getCopyBinary, putCopyBinary)

-- | A script for inclusion into a transaction.
type Script = PLCore.Program

-- | Parse a script intended to serve as a validator (or “lock”) in a
-- transaction output.
parseValidator :: Text -> Either String Script
parseValidator t = PL.loadValidator (toString t)

-- | Parse a script intended to serve as a redeemer (or “proof”) in a
-- transaction input.
parseRedeemer :: Text -> Either String Script
parseRedeemer t = PL.loadRedeemer (toString t)

{-

-- | The type for errors that can appear when validating a script-protected
-- transaction.
data TxScriptError
      -- | The validator doesn't provide a @validator@ function.
    = InvalidValidator
      -- | The redeemer doesn't provide a @redeemer@ function.
    | InvalidRedeemer
      -- | The validator and the redeemer have incompatible types and can't
      -- be combined.
    | TypeMismatch
      -- | Everything typechecks but the result of evaluation isn't @success@
      -- and so the transaction is invalid.
    | ValidationFail

-}

type TxScriptError = String

-- | Validate a transaction, given a validator and a redeemer.
txScriptCheck
    :: Script                     -- ^ Validator
    -> Script                     -- ^ Redeemer
    -> Either TxScriptError ()
txScriptCheck validator redeemer = do
    (script, env) <- PL.buildValidationScript validator redeemer
    result <- PL.checkValidationResult (script, env)
    if result then Right () else Left "result of evaluation is 'failure'"

----------------------------------------------------------------------------
-- Orphan instances, to be included into plutus-prototype
----------------------------------------------------------------------------

deriving instance Show PLCore.PrimData

deriving instance (Show a, Show (PLCore.ClauseF a)) => Show (PLCore.TermF a)
deriving instance (Eq a, Eq (PLCore.ClauseF a)) => Eq (PLCore.TermF a)

deriving instance Eq PLTypes.TyConSig
deriving instance Eq PLTypes.ConSig
deriving instance Eq PLCore.TermDeclaration
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
instance Binary a => Binary (Names.Sourced a)
instance Binary ABT.Variable
instance Binary (f (ABT.Scope f)) => Binary (ABT.ABT f)
instance Binary (f (ABT.Scope f)) => Binary (ABT.Scope f)
instance Binary r => Binary (PLCore.ClauseF r)
instance Binary a => Binary (PLCore.TermF a)
instance Binary a => Binary (PLCore.PatternF a)
instance Binary a => Binary (PLTypes.TypeF a)
instance Binary PLCore.TermDeclaration
instance Binary PLCore.PrimData
instance Binary PLCore.Program

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
instance NFData a => NFData (PLCore.PatternF a)
instance NFData a => NFData (PLTypes.TypeF a)
instance NFData PLCore.TermDeclaration
instance NFData PLCore.PrimData
instance NFData Script

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
