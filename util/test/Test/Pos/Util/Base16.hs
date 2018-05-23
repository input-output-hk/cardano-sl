module Test.Pos.Util.Base16
    ( decode
    , encode
    , encodeWithIndex
    ) where

import           Universum

import qualified Data.Attoparsec.ByteString.Char8 as PBC
import qualified Data.Attoparsec.ByteString.Lazy as PLB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy.Char8 as LB
import           Text.Printf (printf)

----------------------------------------------------------------------------
-- Encoding
----------------------------------------------------------------------------

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
    | len <= 0xff      = 2
    | len <= 0xfff     = 3
    | len <= 0xffff    = 4
    | len <= 0xfffff   = 5
    | len <= 0xffffff  = 6
    | len <= 0xfffffff = 7
    | otherwise        = 8

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
    Just _  ->
        let (taken, dropped) = LB.splitAt n xs
        in  taken : chunkBS n dropped

----------------------------------------------------------------------------
-- Decoding
----------------------------------------------------------------------------

-- | Decode a given ByteString which was originally encoded using 'encode' or
-- 'encodeWithIndex'.
decode :: LB.ByteString -> Maybe LB.ByteString
decode bs
    -- No complex parsing is required for data whose length is <= 32.
    | LB.length bs <= lineWrapLength = Just $ fst $ B16.decode bs
    | otherwise = case PLB.maybeResult $ PLB.parse decodeParser bs of
        Nothing -> Nothing
        Just r  -> Just $ fst $ B16.decode $ LB.fromStrict $ BC.concat r

-- | Parser for several lines of data encoded using 'encode' or
-- 'encodeWithIndex'.
decodeParser :: PBC.Parser [ByteString]
decodeParser = many $ encodedEntryParser <* PBC.endOfLine

-- | Parser for a single entry in a series of data encoded using 'encode' or
-- 'encodeWithIndex'.
encodedEntryParser :: PBC.Parser ByteString
encodedEntryParser = do
    _ <- PBC.hexadecimal :: PBC.Parser Int    -- Read the byte offset
    PBC.skipWhile (not . PBC.isSpace)         -- Skip until whitespace
    PBC.skipSpace                             -- Skip the whitespace
    PBC.takeWhile (not . (`BC.elem` "\n\r"))  -- Consume the data up until LF
                                              -- or CR.
