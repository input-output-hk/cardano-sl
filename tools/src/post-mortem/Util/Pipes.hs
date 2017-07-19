module Util.Pipes
    ( parseP
    , fold'
    ) where

import           Control.Foldl              (Fold (..))
import           Data.Attoparsec.ByteString (Parser, parse, Result, IResult (..))
import           Pipes
import qualified Pipes.Prelude              as P

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

fold' :: Monad m => Fold a b -> Producer a m () -> m b
fold' (Fold step begin extract) = P.fold step begin extract
