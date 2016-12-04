-- | Basic serialization instances for @Bi@ typeclass

module Pos.Binary.Basic where

import qualified Data.Binary      as Binary
import qualified Data.ByteString  as BS

import           Pos.Binary.Class (Bi (..))

instance Bi BS.ByteString where
    put = Binary.put
    get = Binary.get
