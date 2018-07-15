module Pos.Util.OptParse
       ( fromParsec
       ) where

import           Universum

import           Options.Applicative (ReadM, eitherReader)
import           Text.Megaparsec (Parsec, parse)

fromParsec :: Parsec () String a -> ReadM a
fromParsec parser =
    eitherReader $ first show . parse parser "<CLI options>"
