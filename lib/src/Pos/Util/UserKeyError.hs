-- | Possible errors with files which stores secret and public keys.

module Pos.Util.UserKeyError
       ( UserPublicError (..)
       , UserSecretError (..)
       ) where

import           Universum

import           Control.Exception.Safe (Exception)

-- | Represents errors which can occur with file that stores extended public keys (for external wallets).
data UserPublicError
    = UserPublicNotWritable
    | UserPublicAlreadyLocked
    | UserPublicIncorrectLock
    deriving (Show)

-- | Represents errors which can occur with file that stores secret keys (for regular wallets).
data UserSecretError
    = UserSecretNotWritable
    | UserSecretAlreadyLocked
    | UserSecretIncorrectLock
    deriving (Show)

instance Exception UserPublicError
instance Exception UserSecretError
