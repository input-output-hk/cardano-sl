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
import           Pos.Script.Type       (Script)
import           Pos.Util              (getCopyBinary, putCopyBinary)

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
