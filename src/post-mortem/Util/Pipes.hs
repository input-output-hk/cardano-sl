module Util.Pipes
    ( parseP
    ) where

import Data.Attoparsec.ByteString (Parser, parse, Result, IResult (..))
import Pipes

import Universum

parseP :: forall m a b. Monad m => Parser a -> Pipe ByteString a m b
parseP p = go (parse p)

  where

    go :: (ByteString -> Result a) -> Pipe ByteString a m b
    go p' = await >>= consume p'

    consume :: (ByteString -> Result a) -> ByteString -> Pipe ByteString a m b
    consume p' chunk = case p' chunk of
        Fail _ _ e    -> error $ toText e
        Done chunk' a -> yield a >> consume (parse p) chunk'
        Partial p''   -> go p''
