{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

-- | A wrapper over Plutus (the scripting language used in transactions).
module Pos.Script
       ( Script(..)
       , TxScriptError

       , txScriptCheck

       , parseValidator
       , parseRedeemer

       , stdlib

       , isKnownScriptVersion
       ) where

import           Control.Exception          (ArithException (..), ArrayException (..),
                                             ErrorCall (..), Handler (..),
                                             PatternMatchFail (..), SomeException (..),
                                             catches, displayException, throwIO)
import qualified Interface.Integration      as PL
import qualified Interface.Prelude          as PL
import           Language.Haskell.TH.Syntax (Lift (..), runIO)
import qualified PlutusCore.Program         as PLCore
import           System.IO.Unsafe           (unsafePerformIO)
import           Universum                  hiding (lift)

import           Pos.Binary.Class           (Bi)
import qualified Pos.Binary.Class           as Bi
import           Pos.Binary.Crypto          ()
import           Pos.Core.Script            ()
import           Pos.Core.Types             (Script (..), ScriptVersion, Script_v0)
import           Pos.Txp.Core.Types         (TxSigData)

{- NOTE

Scripts are versioned. The current version is 0. All functions below work
with version 0 scripts.

Here's what would lead to script version increment:
  * changing serialization in any way
  * adding anything to the stdlib
-}

isKnownScriptVersion :: ScriptVersion -> Bool
isKnownScriptVersion v = v == 0

-- | Parse a script intended to serve as a validator (or “lock”) in a
-- transaction output.
parseValidator :: Bi Script_v0 => Text -> Either String Script
parseValidator t = do
    scr <- PL.runElabInContexts [stdlib] $ PL.loadValidator (toString t)
    return Script {
        scrScript = Bi.encode scr,
        scrVersion = 0 }

-- | Parse a script intended to serve as a redeemer (or “proof”) in a
-- transaction input.
--
-- Can be given an optional validator (e.g. if the redeemer uses functions or
-- types defined by the validator).
parseRedeemer :: Bi Script_v0 => Maybe Script -> Text -> Either String Script
parseRedeemer mbV t = do
    mbValScr <- case (\x -> (scrVersion x, x)) <$> mbV of
        Nothing       -> return Nothing
        Just (0, val) -> Just <$> Bi.decodeFull (scrScript val)
        Just (v, _)   -> Left ("unknown script version of validator: " ++
                               show v)
    scr <- PL.runElabInContexts (stdlib : maybeToList mbValScr) $
               PL.loadRedeemer (toString t)
    return Script {
        scrScript = Bi.encode scr,
        scrVersion = 0 }

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
      -- | Some error (like an 'error' being thrown by script evaluator)
    | Exception
      -- | Everything typechecks but the result of evaluation isn't @success@
      -- and so the transaction is invalid.
    | ValidationFail

-}

type TxScriptError = String

-- | Validate a transaction, given a validator and a redeemer.
txScriptCheck
    :: Bi Script_v0
    => TxSigData
    -> Script                     -- ^ Validator
    -> Script                     -- ^ Redeemer
    -> Either TxScriptError ()
txScriptCheck sigData validator redeemer = case spoon result of
    Left x              -> Left ("exception when evaluating a script: " ++ x)
    Right (Left x)      -> Left x
    Right (Right False) -> Left "result of evaluation is 'failure'"
    Right (Right True)  -> Right ()
  where
    result :: Either String Bool
    result = do
        -- TODO: when we support more than one version, complain if versions
        -- don't match
        valScr <- case scrVersion validator of
            0 -> Bi.decodeFull (scrScript validator)
            v -> Left ("unknown script version of validator: " ++ show v)
        redScr <- case scrVersion redeemer of
            0 -> Bi.decodeFull (scrScript redeemer)
            v -> Left ("unknown script version of redeemer: " ++ show v)
        (script, env) <- PL.buildValidationScript stdlib valScr redScr
        PL.checkValidationResult (Bi.encode sigData) (script, env)

stdlib :: PLCore.Program
stdlib = $(do
    pr <- runIO PL.prelude
    lift pr)

----------------------------------------------------------------------------
-- Error catching
----------------------------------------------------------------------------

{-# INLINEABLE defaultHandles #-}
defaultHandles :: [Handler (Either String a)]
defaultHandles =
    [ Handler $ \(x :: ArithException)   -> return (Left (displayException x))
    , Handler $ \(x :: ArrayException)   -> return (Left (displayException x))
    , Handler $ \(x :: ErrorCall)        -> return (Left (displayException x))
    , Handler $ \(x :: PatternMatchFail) -> return (Left (displayException x))
    , Handler $ \(x :: SomeException)    -> throwIO x ]

{-# INLINE spoon #-}
spoon :: NFData a => a -> Either String a
spoon a = unsafePerformIO $
    deepseq a (Right `fmap` return a) `catches` defaultHandles
