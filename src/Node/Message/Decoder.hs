module Node.Message.Decoder
    ( Decoder (..)
    , ByteOffset
    , continueDecoding
    ) where

import           Data.Int         (Int64)
import qualified Data.ByteString  as BS
import qualified Data.Text        as T

type ByteOffset = Int64

data Decoder t =
      Done !BS.ByteString !ByteOffset !t
    | Fail !BS.ByteString !ByteOffset !T.Text
    | Partial (Maybe BS.ByteString -> Decoder t)

continueDecoding
    :: Decoder t
    -> BS.ByteString
    -> Decoder t
continueDecoding decoder bs = case decoder of
    Done trailing offset t -> Done (BS.append trailing bs) offset t
    Fail trailing offset err -> Fail (BS.append trailing bs) offset err
    Partial k -> k $ Just bs
