{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Pos.Util
       ( binaryEncodeDecode
       , safeCopyEncodeDecode
       , serDeserId
       , showRead
       ) where

import           Data.Binary      (Binary)
import qualified Data.Binary      as Binary
import           Data.SafeCopy    (SafeCopy, safeGet, safePut)
import           Data.Serialize   (runGet, runPut)
import           Pos.Util         (Serialized (..))
import           Prelude          (read)
import           Test.QuickCheck  (Property, (===))
import           Universum

binaryEncodeDecode :: (Show a, Eq a, Binary a) => a -> Property
binaryEncodeDecode a = Binary.decode (Binary.encode a) === a

safeCopyEncodeDecode :: (Show a, Eq a, SafeCopy a) => a -> Property
safeCopyEncodeDecode a =
    either (panic . toText) identity
     (runGet safeGet $ runPut $ safePut a) === a

showRead :: (Show a, Eq a, Read a) => a -> Property
showRead a = read (show a) === a

serDeserId :: forall t . (Show t, Eq t, Serialized t) => t -> Property
serDeserId a =
    let serDeser = either (panic . toText) identity . deserialize . serialize @t
    in a === serDeser a
