{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
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

import           Control.Exception          (ArithException (..), ArrayException (..),
                                             ErrorCall (..), Handler (..),
                                             PatternMatchFail (..), SomeException (..),
                                             catches, displayException, throwIO)
import           Control.Monad.Fail         (fail)
import           Data.FileEmbed             (embedStringFile, makeRelativeToProject)
import           Data.String                (String)
import qualified Interface.Integration      as PL
import           Language.Haskell.TH.Syntax (Lift (..))
import qualified PlutusCore.Program         as PLCore
import           System.IO.Unsafe           (unsafePerformIO)
import           Universum                  hiding (lift)

import           Pos.Binary.Class           (Bi)
import qualified Pos.Binary.Class           as Bi
import           Pos.Script.Type            (Script (..), Script_v0)

{- NOTE

Scripts are versioned. The current version is 0. All functions below work
with version 0 scripts.

Here's what would lead to script version increment:
  * changing serialization in any way
  * adding anything to the stdlib
-}

-- | Parse a script intended to serve as a validator (or “lock”) in a
-- transaction output.
parseValidator :: Bi Script_v0 => Text -> Either String Script
parseValidator t = do
    scr <- PL.runElabInContext stdlib $ PL.loadValidator (toString t)
    return Script {
        scrScript = Bi.encode scr,
        scrVersion = 0 }

-- | Parse a script intended to serve as a redeemer (or “proof”) in a
-- transaction input.
parseRedeemer :: Bi Script_v0 => Text -> Either String Script
parseRedeemer t = do
    scr <- PL.runElabInContext stdlib $ PL.loadRedeemer (toString t)
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
    => Script                     -- ^ Validator
    -> Script                     -- ^ Redeemer
    -> Either TxScriptError ()
txScriptCheck validator redeemer = case spoon result of
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
        PL.checkValidationResult (script, env)

stdlib :: PLCore.Program
stdlib = $(do
    let file = $(embedStringFile =<< makeRelativeToProject "stdlib.pls")
    case PL.runElabNoContext (PL.loadProgram file) of
        Left a  -> fail ("couldn't parse script standard library: " ++ a)
        Right x -> lift x)

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
