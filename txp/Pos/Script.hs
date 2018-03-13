{-# LANGUAGE DeriveAnyClass #-}

-- | A wrapper over Plutus (the scripting language used in transactions).

module Pos.Script
       ( Script(..)
       , PlutusError(..)

       , txScriptCheck

       , parseValidator
       , parseRedeemer

       , stdlib

       , isKnownScriptVersion
       ) where

import           Universum hiding (lift)

import           Control.Exception (ArithException (..), ArrayException (..), ErrorCall (..),
                                    PatternMatchFail (..))
import           Control.Exception.Safe (Handler (..), SomeException (..), catches,
                                         displayException)
import           Control.Lens (_Left)
import           Control.Monad.Error.Class (throwError)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as S
import qualified Data.Text.Buildable as Buildable
import qualified Elaboration.Contexts as PL
import qualified Interface.Integration as PL
import qualified Interface.Prelude as PL
import           Language.Haskell.TH.Syntax (Lift (..), runIO)
import qualified PlutusCore.EvaluatorTypes as PLCore
import qualified PlutusCore.Program as PL
import           System.IO.Unsafe (unsafePerformIO)
import qualified Utils.Names as PL

import qualified Pos.Binary.Class as Bi
import           Pos.Binary.Core ()
import           Pos.Binary.Crypto ()
import           Pos.Core.Common (Script (..), ScriptVersion)
import           Pos.Core.Script ()
import           Pos.Core.Txp (TxSigData (..))

{- NOTE

Scripts are versioned. The current version is 0. All functions below work
with version 0 scripts.

Here's what would lead to script version increment:
  * changing serialization in any way
  * adding anything to the stdlib
-}

isKnownScriptVersion :: ScriptVersion -> Bool
isKnownScriptVersion v = v == 0

-- | Post-process loaded program to remove stdlib references that were added
-- to the environment by 'loadValidator' or 'loadRedeemer'.
stripStdlib :: PL.Program -> PL.Program
stripStdlib (PL.Program xs) = PL.Program (filter (not . std) xs)
  where
    stds = S.fromList (map (PL.unsourced . fst) (PL.definitions stdlib))
    std (name, _) = PL.unsourced name `elem` stds

-- | Parse a script intended to serve as a validator (or “lock”) in a
-- transaction output.
parseValidator :: Text -> Either String Script
parseValidator t = do
    scr <- stripStdlib <$> PL.loadValidator stdlib (toString t)
    return Script {
        scrScript = Bi.serialize' scr,
        scrVersion = 0 }

-- | Parse a script intended to serve as a redeemer (or “proof”) in a
-- transaction input.
parseRedeemer :: Text -> Either String Script
parseRedeemer t = do
    scr <- stripStdlib <$> PL.loadRedeemer stdlib (toString t)
    return Script {
        scrScript = Bi.serialize' scr,
        scrVersion = 0 }

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

-- | Validate a transaction, given a validator and a redeemer.
txScriptCheck
    :: TxSigData
    -> Script                     -- ^ Validator
    -> Script                     -- ^ Redeemer
    -> Either PlutusError ()
txScriptCheck sigData validator redeemer = case spoon result of
    Left err            -> throwError (PlutusException (toText err))
    Right (Left err)    -> throwError err
    Right (Right False) -> throwError PlutusReturnedFalse
    Right (Right True)  -> pass
  where
    result :: Either PlutusError Bool
    result = do
        -- TODO: when we support more than one version, complain if versions
        -- don't match
        valScr <- case scrVersion validator of
            0 -> over _Left PlutusDecodingFailure $
                     Bi.decodeFull' (scrScript validator)
            v -> Left (PlutusUnknownVersion v)
        redScr <- case scrVersion redeemer of
            0 -> over _Left PlutusDecodingFailure $
                     Bi.decodeFull' (scrScript redeemer)
            v -> Left (PlutusUnknownVersion v)
        (script, env) <- over _Left (PlutusExecutionFailure . toText) $
            PL.buildValidationScript stdlib valScr redScr
        let txInfo = PLCore.TransactionInfo
                { txHash      = BSL.fromStrict . BA.convert $
                                txSigTxHash sigData }
        over _Left (PlutusExecutionFailure . toText) $
            PL.checkValidationResult txInfo (script, env)

stdlib :: PL.DeclContext
stdlib = case PL.loadLibrary PL.emptyDeclContext prelude of
    Right x  -> x
    Left err -> error $ toText
                  ("stdlib: error while parsing Plutus prelude: " ++ err)
  where
    prelude = $(lift . toString =<< runIO PL.preludeString)

----------------------------------------------------------------------------
-- Error catching
----------------------------------------------------------------------------

{-# INLINEABLE defaultHandles #-}
defaultHandles :: [Handler IO (Either String a)]
defaultHandles =
    [ Handler $ \(x :: ArithException)   -> return (Left (displayException x))
    , Handler $ \(x :: ArrayException)   -> return (Left (displayException x))
    , Handler $ \(x :: ErrorCall)        -> return (Left (displayException x))
    , Handler $ \(x :: PatternMatchFail) -> return (Left (displayException x))
    , Handler $ \(x :: SomeException)    -> throwM x ]

{-# INLINE spoon #-}
spoon :: NFData a => a -> Either String a
spoon a = unsafePerformIO $
    deepseq a (Right `fmap` return a) `catches` defaultHandles
