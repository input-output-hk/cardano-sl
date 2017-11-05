module Bench.Pos.Criterion.BlockSerialization
       ( runBenchmark
       ) where

import           Universum

import           Control.Monad.IO.Class     (liftIO)
import           Criterion
import           Criterion.Main
import qualified Data.ByteString            as BS
import           Data.Default               (def)
import qualified Data.Text                  as T
import           Mockable.Production        (runProduction)
import           Test.QuickCheck.Arbitrary  (arbitrary)
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random

import           Pos.Arbitrary.Block        ()
import           Pos.Arbitrary.Ssc          ()
import           Pos.Binary                 (decodeFull, decodeFullNoCheck, serialize')
import           Pos.Block.Core             (Block, MainBlock)
import           Pos.Core                   (Timestamp (..))
import           Pos.Launcher               (LoggingParams (..), loggerBracket)
import           Pos.Launcher.Configuration (ConfigurationOptions (..), HasConfigurations,
                                             withConfigurations)

runBenchmark :: IO ()
runBenchmark =
    runProduction $
    withConfigurations conf $ liftIO (defaultMain [serializationBenchmark])

conf :: ConfigurationOptions
conf = def { cfoSystemStart = Just (Timestamp 1234), cfoFilePath = "configuration.yaml" }

serializationBenchmark :: HasConfigurations => Benchmark
serializationBenchmark =
    env generateBlock $ \(~(block, serialized)) ->
        bgroup
            "Block serialization"
            [ bgroup "Encoding" [bench "encode" (nf serialize' block)]
            , bgroup
                  "Decoding"
                  [ bench "verified" (nf decodeChecked serialized)
                  , bench "unverified" (nf decodeUnchecked serialized)
                  ]
            , bgroup
                  "Decoding (throw away value)"
                  [ bench
                        "verified"
                        (nf (void . decodeChecked) serialized)
                  , bench
                        "unverified"
                        (nf (void . decodeUnchecked) serialized)
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
    loggingParams =
        LoggingParams
        { lpRunnerTag = "benchmark"
        , lpHandlerPrefix = Nothing
        , lpConfigPath = Nothing
        , lpConsoleLog = Just False
        }
