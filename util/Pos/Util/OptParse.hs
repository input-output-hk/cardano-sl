module Pos.Util.OptParse
       ( fromParsec
       ) where

import           Universum

import           Options.Applicative (ReadM, eitherReader)
import           Text.Megaparsec (Parsec, parse)

fromParsec :: Show e => Parsec e Text a -> ReadM a
fromParsec parser =
    eitherReader $ first show . parse parser "<CLI options>" . toText
