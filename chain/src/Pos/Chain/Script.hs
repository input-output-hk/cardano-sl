{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

-- | A wrapper over Plutus (the scripting language used in transactions).

module Pos.Chain.Script
       ( Script(..)
       , PlutusError(..)
       ) where

import           Universum

import qualified Formatting.Buildable as Buildable

import           Pos.Core.Common (Script (..), ScriptVersion)


-- | The type for errors that can appear when validating a script-protected
-- transaction.
data PlutusError
    -- | A script has a version that we don't know how to execute.
    = PlutusUnknownVersion ScriptVersion
    -- | A script couldn't be deserialized.
    | PlutusDecodingFailure Text
    -- | The script evaluator refused to execute the program (e.g. it
    -- doesn't typecheck, or the evaluator has run out of petrol).
    | PlutusExecutionFailure Text
    -- | The script evaluator threw an __exception__ (e.g. with 'error').
    | PlutusException Text
    -- | Everything typechecks and executes just fine but the result of
    -- evaluation is @failure@ and so the transaction is invalid.
    | PlutusReturnedFalse
    deriving (Eq, Show, Generic, NFData)

instance Buildable PlutusError where
    build (PlutusUnknownVersion v) =
        "unknown script version: " <> Buildable.build v
    build (PlutusDecodingFailure s) =
        "script decoding failure: " <> Buildable.build s
    build (PlutusExecutionFailure s) =
        "script execution failure: " <> Buildable.build s
    build (PlutusException s) =
        "Plutus threw an exception: " <> Buildable.build s
    build PlutusReturnedFalse =
        "script execution resulted in 'failure'"

