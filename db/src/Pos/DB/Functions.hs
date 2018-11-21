{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Different key/value serialization helpers abstracted over
-- 'MonadDB'.

module Pos.DB.Functions
       (
       -- * Encoded putting/getting
         dbGetBi
       , dbPutBi

       -- * Decoding/encoding primitives and iteration related
       , encodeWithKeyPrefix
       , processIterEntry
       ) where

import           Pos.DB.Rocks
