module Main where

import qualified Data.ByteString.Lazy as BSL
import           Formatting           (sformat)
import           Pos.Crypto           (hashHexF)
import           Pos.Update           (installerHash)
import           System.Environment   (getArgs)
import           Universum            hiding (getArgs)

main :: IO ()
main = do
  [path] <- getArgs
  h <- installerHash <$> BSL.readFile path
  putText $ sformat (hashHexF) h
