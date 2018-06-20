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
import           Pos.Core (Address, BlockVersion, SoftwareVersion, StakeholderId)
import           Pos.Crypto (AHash, PublicKey)

data Expr cmd
    = ExprUnit
    | ExprGroup (NonEmpty (Expr cmd))
    | ExprProcCall (ProcCall cmd (Expr cmd))
    | ExprLit Lit

deriving instance Eq cmd => Eq (Expr cmd)
deriving instance Ord cmd => Ord (Expr cmd)
deriving instance Show cmd => Show (Expr cmd)

data Lit
    = LitNumber Scientific
    | LitString Text
    | LitAddress Address
    | LitPublicKey PublicKey
    | LitStakeholderId StakeholderId
    | LitHash AHash
    | LitBlockVersion BlockVersion
    | LitSoftwareVersion SoftwareVersion
    | LitFilePath FilePath
    deriving (Eq, Ord, Show)

data ProcCall cmd a = ProcCall cmd [Arg a]
    deriving (Functor, Foldable, Traversable)

deriving instance (Eq cmd, Eq a) => Eq (ProcCall cmd a)
deriving instance (Ord cmd, Ord a) => Ord (ProcCall cmd a)
deriving instance (Show cmd, Show a) => Show (ProcCall cmd a)

data Arg a = ArgPos a | ArgKw Name a
    deriving (Functor, Foldable, Traversable)

deriving instance Eq a => Eq (Arg a)
deriving instance Ord a => Ord (Arg a)
deriving instance Show a => Show (Arg a)
