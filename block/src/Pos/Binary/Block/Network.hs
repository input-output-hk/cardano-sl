-- | Network and block processing related communication stuff.

module Pos.Binary.Block.Network
       ( msgBlockPrefix
       ) where

import           Universum

import qualified Codec.CBOR.Write as CBOR (toStrictByteString)

import           Pos.Binary.Class (Bi (..), encodeListLen)

-- | Get an encoded MsgBlock from an encoded block.
msgBlockPrefix :: ByteString
msgBlockPrefix = CBOR.toStrictByteString prefix
  where
    prefix = encodeListLen 2 <> encode (0 :: Word8)
