{-# LANGUAGE ExistentialQuantification #-}

module Lang.Command
       ( CommandProc(..)
       , UnavailableCommand(..)
       , resolveCommandProcs
       ) where

import           Universum

import           Control.Lens (_Left)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Validation as Validation

import           Lang.Argument (ArgumentConsumer)
import           Lang.Name (Name)
import           Lang.Syntax (Arg, Expr (..), ProcCall (..))
import           Lang.Value (Value)

data CommandProc m = forall e. CommandProc
    { cpName             :: !Name
    , cpArgumentPrepare  :: !([Arg Value] -> [Arg Value])
    , cpArgumentConsumer :: !(ArgumentConsumer e)
    , cpExec             :: !(e -> m Value)
    , cpHelp             :: !Text
    } deriving ()

data UnavailableCommand = UnavailableCommand
    { ucName   :: !Name
    , ucReason :: !Text
    }

resolveCommandProcs ::
    [CommandProc m] ->
    Expr Name ->
    Either (NonEmpty Name) (Expr (CommandProc m))
resolveCommandProcs commandProcs =
    over _Left NonEmpty.nub . Validation.toEither . go
  where
    go ExprUnit                = pure ExprUnit
    go (ExprLit l)             = pure (ExprLit l)
    go (ExprGroup exprs)       = ExprGroup <$> traverse go exprs
    go (ExprProcCall procCall) = ExprProcCall <$> goProcCall procCall

    goProcCall (ProcCall procName args) =
        ProcCall
            <$> lookupProcName procName
            <*> (traverse.traverse) go args

    lookupProcName procName =
        Validation.fromEither $
        note (procName :| []) $
        find (\cp -> cpName cp == procName) commandProcs
