{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

-- | Binary serialization of Pos.Types.* modules

module Pos.Binary.Util
  ( getAsciiString1b
  , putAsciiString1b
  ) where

import           Control.Monad.Fail (fail)
import           Data.Binary.Get    (Get, getByteString, getWord8)
import           Data.Binary.Put    (Put, putByteString, putWord8)
import qualified Data.ByteString    as BS
import           Data.Char          (chr, isAscii, ord)
import           Universum          hiding (putByteString)

getAsciiString1b :: [Char] -> Word8 -> Get [Char]
getAsciiString1b typeName limit = getWord8 >>= \sz -> do
            if sz > limit
               then fail $ typeName ++ " shouldn't be more than "
                                    ++ show limit ++ " bytes long"
               else traverse checkAscii =<< BS.unpack <$> getByteString (fromIntegral sz)
  where
    checkAscii (chr . fromIntegral -> c) =
        if isAscii c
           then return c
           else fail $ "Not an ascii symbol in " ++ typeName ++ " " ++ show c

putAsciiString1b :: [Char] -> Put
putAsciiString1b str =  putWord8 (fromIntegral $ length str)
                     >> putByteString (BS.pack $ map (fromIntegral . ord) str)
