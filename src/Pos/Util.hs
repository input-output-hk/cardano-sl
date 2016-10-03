{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Pos.Util
       ( Raw
       ) where

import           Data.ByteString (ByteString)
import           Universum

-- | A wrapper over 'ByteString' for adding type safety to
-- 'Pos.Crypto.Pki.encryptRaw' and friends.
newtype Raw = Raw ByteString
    deriving (Eq, Ord, Show)
