-- | All errors possible in block-gen.

module Error
       ( TBlockGenError (..)
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting          (bprint, build, (%))

data TBlockGenError
    = NoOneSecrets
    | NodeCountAndSecrets
    | SecretNotFound !FilePath
    | AppendToNonexistDB
    | EmptyUtxo
    deriving (Show)

instance Exception TBlockGenError

instance Buildable TBlockGenError where
    build NoOneSecrets = bprint "You passed no one secrets"
    build NodeCountAndSecrets = bprint "You passed a node count and secret files\
                                       \  Please give one or the other but not both."
    build (SecretNotFound p) =
        bprint ("There is no secret at the path "%build) p
    build AppendToNonexistDB =
        bprint "You specified --append flag, but DB doesn't exist"
    build EmptyUtxo =
        bprint "There is no addresses in the genesis utxo corresponding passed secrets"
