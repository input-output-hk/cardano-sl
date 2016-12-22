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

       , stdlib
       ) where

import           Control.Monad.Fail         (fail)
import           Data.FileEmbed             (embedStringFile, makeRelativeToProject)
import           Data.String                (String)
import qualified Interface.Integration      as PL
import           Language.Haskell.TH.Syntax (Lift (..))
import qualified PlutusCore.Program         as PLCore
import           Universum                  hiding (lift)

import           Pos.Script.Type            (Script)

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
    (script, env) <- PL.buildValidationScript stdlib validator redeemer
    result <- PL.checkValidationResult (script, env)
    if result then Right () else Left "result of evaluation is 'failure'"

stdlib :: PLCore.Program
stdlib =
    $(do let file = $(embedStringFile =<< makeRelativeToProject "stdlib.pls")
         case PL.loadProgram file of
             Left a  -> fail ("couldn't parse script standard library: " ++ a)
             Right x -> lift x)
