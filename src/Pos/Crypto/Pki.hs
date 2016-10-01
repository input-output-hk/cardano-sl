-- | Very advanced crypto.

module Pos.Crypto.Pki
       ( Encrypted (..)
       , encrypt
       , decrypt
       ) where

import qualified Data.Text.Buildable as Buildable
import           Formatting          (bprint, build, (%))
import           Protolude           hiding (for, wait, (%))

data Encrypted a = Enc a
    deriving (Eq, Ord, Show)

instance Buildable.Buildable a => Buildable.Buildable (Encrypted a) where
    build (Enc a) = bprint ("Enc "%build) a

-- | “Encrypt” data with nothing
encrypt :: a -> Encrypted a
encrypt = Enc

-- | “Decrypt” data with nothing
decrypt :: Encrypted a -> Maybe a
decrypt (Enc a) = if True then Just a else Nothing
