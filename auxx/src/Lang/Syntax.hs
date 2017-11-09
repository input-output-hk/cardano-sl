{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module Lang.Syntax
       ( Expr(..)
       , Lit(..)
       , ProcCall(..)
       , Arg(..)
       ) where

import           Universum

import           Data.Scientific (Scientific)

import           Lang.Name (Name)
import           Pos.Crypto (AHash, PublicKey)
import           Pos.Types (Address, BlockVersion, SoftwareVersion, StakeholderId)

data Expr
    = ExprUnit
    | ExprGroup (NonEmpty Expr)
    | ExprProcCall (ProcCall Expr)
    | ExprLit Lit
    deriving (Eq, Ord, Show)

data Lit
    = LitNumber Scientific
    | LitString String
    | LitAddress Address
    | LitPublicKey PublicKey
    | LitStakeholderId StakeholderId
    | LitHash AHash
    | LitBlockVersion BlockVersion
    | LitSoftwareVersion SoftwareVersion
    | LitFilePath FilePath
    deriving (Eq, Ord, Show)

data ProcCall a = ProcCall Name [Arg a]
    deriving (Functor, Foldable, Traversable)

deriving instance Eq a => Eq (ProcCall a)
deriving instance Ord a => Ord (ProcCall a)
deriving instance Show a => Show (ProcCall a)

data Arg a = ArgPos a | ArgKw Name a
    deriving (Functor, Foldable, Traversable)

deriving instance Eq a => Eq (Arg a)
deriving instance Ord a => Ord (Arg a)
deriving instance Show a => Show (Arg a)
