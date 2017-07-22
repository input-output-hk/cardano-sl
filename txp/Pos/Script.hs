{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Data.ByteArray             as BA
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Set                   as S
import qualified Elaboration.Contexts       as PL
import qualified Interface.Integration      as PL
import qualified Interface.Prelude          as PL
import           Language.Haskell.TH.Syntax (Lift (..), runIO)
import qualified PlutusCore.EvaluatorTypes  as PLCore
import qualified PlutusCore.Program         as PL
import           System.IO.Unsafe           (unsafePerformIO)
import           Universum                  hiding (lift)
import qualified Utils.Names                as PL

import           Pos.Binary.Class           (Bi)
import qualified Pos.Binary.Class           as Bi
import           Pos.Binary.Crypto          ()
import           Pos.Binary.Txp.Core        ()
import           Pos.Core.Script            ()
import           Pos.Core.Types             (Script (..), ScriptVersion, Script_v0)
import           Pos.Txp.Core.Types         (TxSigData (..))

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
parseValidator :: Bi Script_v0 => Text -> Either String Script
parseValidator t = do
    scr <- stripStdlib <$> PL.loadValidator stdlib (toString t)
    return Script {
        scrScript = Bi.encodeLazy scr,
        scrVersion = 0 }

-- | Parse a script intended to serve as a redeemer (or “proof”) in a
-- transaction input.
parseRedeemer :: Bi Script_v0 => Text -> Either String Script
parseRedeemer t = do
    scr <- stripStdlib <$> PL.loadRedeemer stdlib (toString t)
    return Script {
        scrScript = Bi.encodeLazy scr,
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
            0 -> first toString $ Bi.decodeFull $ BSL.toStrict $ scrScript validator
            v -> Left ("unknown script version of validator: " ++ show v)
        redScr <- case scrVersion redeemer of
            0 -> first toString $ Bi.decodeFull $ BSL.toStrict (scrScript redeemer)
            v -> Left ("unknown script version of redeemer: " ++ show v)
        (script, env) <- PL.buildValidationScript stdlib valScr redScr
        let txInfo = PLCore.TransactionInfo
                { txHash      = BSL.fromStrict . BA.convert $
                                txSigTxHash sigData
                , txDistrHash = BSL.fromStrict . BA.convert $
                                txSigTxDistrHash sigData }
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
