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

data EvalCtx m = EvalCtx
    { ecCommandProcs :: [CommandProc m]
    } deriving ()

data EvalError
    = CommandNotSupported Name
    | InvalidArguments Name ProcError
    deriving (Eq, Ord, Show)

type T m a = Monad m =>
    ReaderT (EvalCtx m) (ExceptT EvalError m) a

evaluate :: Monad m => [CommandProc m] -> Expr -> m (Either EvalError Value)
evaluate commandProcs expr = runExceptT $ runReaderT (eval expr) ctx
  where
    ctx = EvalCtx { ecCommandProcs = commandProcs }

eval :: Expr -> T m Value
eval = \case
    ExprUnit -> return ValueUnit
    ExprLit l -> return (literalToValue l)
    ExprGroup exprs -> evalExprGroup exprs
    ExprProcCall procCall ->
        evalProcCall =<< traverse eval procCall

-- | Evaluage a group of expressions and take the result
-- of the last one. Equivalent to @fmap NE.last . traverse eval@,
-- but is single-pass.
evalExprGroup :: NonEmpty Expr -> T m Value
evalExprGroup (x :| xs) = case nonEmpty xs of
    Nothing  -> eval x
    Just xs' -> eval x *> evalExprGroup xs'

evalProcCall :: ProcCall Value -> T m Value
evalProcCall (ProcCall procName args) = do
    CommandProc{..} <- lookupCommandProc procName
    e <- either (throwError . InvalidArguments cpName) return $
         consumeArguments cpArgumentConsumer $
         cpArgumentPrepare args
    lift . lift $ cpExec e

lookupCommandProc :: Name -> T m (CommandProc m)
lookupCommandProc name = do
    commandProcs <- asks ecCommandProcs
    maybe (throwError $ CommandNotSupported name) return $
        find (\cp -> cpName cp == name) commandProcs

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
