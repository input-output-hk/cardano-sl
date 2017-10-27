{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

import Criterion
import Criterion.Main
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.Default (def)
import qualified Data.Text as T
import Mockable.Production (runProduction)
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Test.QuickCheck.Arbitrary (arbitrary)

import Pos.Arbitrary.Ssc ()
import Pos.Arbitrary.Block ()
import Pos.Binary (decodeFull, decodeFullNoCheck, serialize')
import Pos.Block.Core (MainBlock, Block)
import Pos.Core (Timestamp (..))
import Pos.Launcher.Configuration (HasConfigurations, withConfigurations, ConfigurationOptions (..))
import Pos.Launcher (LoggingParams (..), loggerBracket)

main :: IO ()
main = runProduction $ withConfigurations conf $ liftIO (defaultMain [serializationBenchmark])

conf :: ConfigurationOptions
conf = def { cfoSystemStart = Just (Timestamp 1234) }

serializationBenchmark :: HasConfigurations => Benchmark
serializationBenchmark = env generateBlock $ \(~(block, serialized)) ->
  bgroup "Block serialization" [
      bgroup "Encoding" [
          bench "encode" (nf serialize' block)
        ]
    , bgroup "Decoding" [
          bench "verified" (nf decodeChecked serialized)
        , bench "unverified" (nf decodeUnchecked serialized)
        ]
    , bgroup "Decoding (throw away value)" [
          bench "verified" (nf (fmap (const ()) . decodeChecked) serialized)
        , bench "unverified" (nf (fmap (const ()) . decodeUnchecked) serialized)
        ]
    ]

  where

  decodeChecked :: BS.ByteString -> Either T.Text Block
  decodeChecked = decodeFull

  decodeUnchecked :: BS.ByteString -> Either T.Text Block
  decodeUnchecked = decodeFullNoCheck

  generateBlock :: IO (Block, BS.ByteString)
  generateBlock =
    loggerBracket loggingParams $
    runProduction $ do
      let block :: Block
          !block = Right arbitraryBlock
          !serialized = serialize' block
      return (block, serialized)

  arbitraryBlock :: HasConfigurations => MainBlock
  arbitraryBlock = unGen arbitrary (mkQCGen 0) 10000

  loggingParams :: LoggingParams
  loggingParams = LoggingParams
    { lpRunnerTag = "benchmark"
    , lpHandlerPrefix = Nothing
    , lpConfigPath = Nothing
    , lpConsoleLog = Just False
    }
