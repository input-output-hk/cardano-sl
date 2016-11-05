module Test.Pos.Util
       ( binaryEncodeDecode
       , msgPackEncodeDecode
       , safeCopyEncodeDecode
       , showRead
       ) where

import           Data.Binary      (Binary)
import qualified Data.Binary      as Binary
import           Data.MessagePack (MessagePack, pack, unpack)
import           Data.SafeCopy    (SafeCopy, safeGet, safePut)
import           Data.Serialize   (runGet, runPut)
import           Prelude          (read)
import           Test.QuickCheck  (Property, (===))
import           Universum

binaryEncodeDecode :: (Show a, Eq a, Binary a) => a -> Property
binaryEncodeDecode a = Binary.decode (Binary.encode a) === a

msgPackEncodeDecode :: (Show a, Eq a, MessagePack a) => a -> Property
msgPackEncodeDecode a = maybe err identity (unpack $ pack a) === a
  where
    err = panic "[MessagePackSpec] Failed MessagePack unpacking!"

safeCopyEncodeDecode :: (Show a, Eq a, SafeCopy a) => a -> Property
safeCopyEncodeDecode a =
    either (panic . toText) identity
     (runGet safeGet $ runPut $ safePut a) === a

showRead :: (Show a, Eq a, Read a) => a -> Property
showRead a = read (show a) === a
