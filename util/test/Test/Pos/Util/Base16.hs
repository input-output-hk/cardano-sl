module Test.Pos.Util.Base16
    ( encode
    , encodeWithIndex
    ) where

import           Universum

import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy.Char8 as LB
import           Text.Printf (printf)

-- | Encodes a given ByteString to base-16 and line wraps every 16 bytes.
encode :: LB.ByteString -> LB.ByteString
encode = lineWrapBS lineWrapLength . B16.encode

-- | Encodes a given ByteString to base-16 and displays it alongside its byte
-- offset (line wrapped every 16 bytes).
encodeWithIndex :: LB.ByteString -> LB.ByteString
encodeWithIndex bs
    -- If the length of the ByteString <= 16 (it hasn't been encoded to base-16
    -- yet so we're not checking for <= 32), then just 'encode' rather than
    -- prepending the byte offsets.
    | LB.length bs <= (lineWrapLength `div` 2) = encode bs
    | otherwise = LB.concat $ go 0 (chunkBS lineWrapLength $ B16.encode bs)
  where
    go :: Int64 -> [LB.ByteString] -> [LB.ByteString]
    go _   []     = []
    go acc (x:xs) =
        let numDigits = numByteOffsetDigits $ LB.length bs
        in  LB.concat
                [ (LB.pack $ printf "%0*x: " numDigits acc)
                , x
                , "\n"
                ] : go (acc + 16) xs

-- | Given the number of bytes of data, determine the number of digits required
-- to represent the base-16 byte offset for a hexdump.
numByteOffsetDigits :: Int64 -> Int64
numByteOffsetDigits len
    | len <= 0  = 0
    | otherwise =
        ceiling ((logBase (2 :: Double) $ fromIntegral len) / (4 :: Double))

-- | The length at which our encoding functions will line wrap. We've chosen a
-- length of 32 because we want only want to display 16 bytes of base-16
-- data per line (2 hex digits represent 1 byte).
lineWrapLength :: Int64
lineWrapLength = 32

-- | Line wraps a ByteString every x bytes.
lineWrapBS :: Int64 -> LB.ByteString -> LB.ByteString
lineWrapBS n s = LB.intercalate "\n" $ chunkBS n s

-- | Divides a ByteString into x-length "chunks".
chunkBS :: Int64 -> LB.ByteString -> [LB.ByteString]
chunkBS n xs = case LB.uncons xs of
    Nothing -> []
    Just _  -> LB.take n xs : chunkBS n (LB.drop n xs)
