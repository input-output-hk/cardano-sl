module Test.Pos.Util
       ( binaryEncodeDecode
       ) where

import           Data.Binary     (Binary)
import qualified Data.Binary     as Binary
import           Test.QuickCheck (Property, (===))
import           Universum

binaryEncodeDecode :: (Show a, Eq a, Binary a) => a -> Property
binaryEncodeDecode a = Binary.decode (Binary.encode a) === a
