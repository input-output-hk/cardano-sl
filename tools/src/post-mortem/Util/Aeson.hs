module Util.Aeson
    ( parseJSONP
    ) where

import           Data.Aeson (FromJSON, Result (..), fromJSON)
import           Data.Aeson.Parser (json)
import           Pipes
import           Pipes.Prelude (map)

import           Universum hiding (map)
import           Util.Pipes (parseP)

parseJSONP:: (FromJSON a, Monad m) => Pipe ByteString a m b
parseJSONP = parseP json >-> map (fromResult . fromJSON)
  where
    fromResult :: Result a -> a
    fromResult (Success a) = a
    fromResult (Error e)   = error $ toText e
