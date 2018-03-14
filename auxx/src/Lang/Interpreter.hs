{-# LANGUAGE RankNTypes #-}

module Lang.Interpreter
       ( Value(..)
       , evaluate
       , EvalError(..)
       ) where

import           Universum

import           Control.Monad.Except (throwError)

import           Lang.Argument (ProcError, consumeArguments)
import           Lang.Command (CommandProc (..))
import           Lang.Name (Name)
import           Lang.Syntax (Expr (..), Lit (..), ProcCall (..))
import           Lang.Value (Value (..))

data EvalError = InvalidArguments Name ProcError
    deriving (Eq, Ord, Show)

type EvalT m a = Monad m => ExceptT EvalError m a

evaluate :: Monad m => Expr (CommandProc m) -> m (Either EvalError Value)
evaluate expr = runExceptT (eval expr)

eval :: Expr (CommandProc m) -> EvalT m Value
eval = \case
    ExprUnit -> return ValueUnit
    ExprLit l -> return (literalToValue l)
    ExprGroup exprs -> evalExprGroup exprs
    ExprProcCall procCall ->
        evalProcCall =<< traverse eval procCall

-- | Evaluage a group of expressions and take the result
-- of the last one. Equivalent to @fmap NE.last . traverse eval@,
-- but is single-pass.
evalExprGroup :: NonEmpty (Expr (CommandProc m)) -> EvalT m Value
evalExprGroup (x :| xs) = case nonEmpty xs of
    Nothing  -> eval x
    Just xs' -> eval x *> evalExprGroup xs'

evalProcCall :: ProcCall (CommandProc m) Value -> EvalT m Value
evalProcCall (ProcCall CommandProc{..} args) = do
    e <- either (throwError . InvalidArguments cpName) return $
         consumeArguments cpArgumentConsumer $
         cpArgumentPrepare args
    lift $ cpExec e

literalToValue :: Lit -> Value
literalToValue = f
  where
    f          (LitNumber a) =          ValueNumber a
    f          (LitString a) =          ValueString a
    f         (LitAddress a) =         ValueAddress a
    f       (LitPublicKey a) =       ValuePublicKey a
    f   (LitStakeholderId a) =   ValueStakeholderId a
    f            (LitHash a) =            ValueHash a
    f    (LitBlockVersion a) =    ValueBlockVersion a
    f (LitSoftwareVersion a) = ValueSoftwareVersion a
    f        (LitFilePath a) =        ValueFilePath a
