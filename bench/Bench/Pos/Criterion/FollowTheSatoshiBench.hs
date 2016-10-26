{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Bench.Pos.Criterion.FollowTheSatoshiBench
  ( runBenchmark
  ) where

import           Criterion.Main        (Benchmark, bench, defaultConfig,
                                        defaultMainWith, env, whnf)
import           Criterion.Types       (Config (..))
import           Data.ByteString.Char8 (pack)
import           Data.DeriveTH         (derive, makeArbitrary, makeNFData)
import           Data.Map              (fromList)
import           Data.Maybe
import           Formatting            (int, sformat, (%))
import           Test.QuickCheck       (Arbitrary (..), Gen, generate,
                                        infiniteList, infiniteListOf)
import           Universum

import           Pos.Crypto            (PublicKey, SecretKey,
                                        deterministicKeyGen, unsafeHash)
import           Pos.FollowTheSatoshi  (followTheSatoshi)
import           Pos.Types             (Address (..), Coin (..), FtsSeed (..),
                                        TxId, TxOut (..), Utxo)
type UtxoSize = Int

-- | Generate arbitrary key pair
--
-- TODO: this is necessary here only to get arbitrary `PublicKey`, and
-- making a valid key pair is an overkill, it's bad for test performance.
--
-- Unfortunately, we don't export constructors for `PublicKey`, and it's
-- impossible to make arbitrary public key without corresponding secret key.
--
-- What about providing `Arbitrary` instance for `PublicKey` inside `Pos.Crypto.Signing`?
arbitraryKeyPair :: Gen (PublicKey, SecretKey)
arbitraryKeyPair = fromJust . deterministicKeyGen . toS . pack . take 32 <$> infiniteList

instance Arbitrary PublicKey where
  arbitrary = fst <$> arbitraryKeyPair

-- Such an instance is necessary, because `Coin` is `Int64` underneath,
-- and therefore can be negative, which sometimes causes `randomNumber`
-- inside `followTheSatoshi` to break
--
-- TODO: what about making `Coin` `Word64` instead? When we might need negative
-- coin amounts?
instance Arbitrary Coin where
  arbitrary = Coin . abs <$> arbitrary

-- TODO: how about putting it all into `Pos.Types.Types`?
-- derive makeArbitrary ''Coin
derive makeArbitrary ''Address
derive makeArbitrary ''TxOut
derive makeArbitrary ''FtsSeed

derive makeNFData ''Coin
derive makeNFData ''Address
derive makeNFData ''TxOut
derive makeNFData ''FtsSeed

arbitraryTxId :: Gen TxId
arbitraryTxId = unsafeHash @ByteString <$> arbitrary

arbitraryUtx :: Gen ((TxId, Word32), TxOut)
arbitraryUtx = (,) <$> ((,) <$> arbitraryTxId <*> arbitrary) <*> arbitrary

arbitraryUtxoOfSize :: UtxoSize -> Gen Utxo
arbitraryUtxoOfSize n = fromList . take n <$> infiniteListOf arbitraryUtx

ftsBench :: UtxoSize -> Benchmark
ftsBench n = env genArgs $ bench msg . whnf (uncurry followTheSatoshi)
  where genArgs = generate $ (,) <$> arbitrary <*> arbitraryUtxoOfSize n
        msg = toS $ sformat ("followTheSatoshi: Utxo of size "%int) n

ftsConfig :: Config
ftsConfig = defaultConfig
  { reportFile = Just "followTheSatoshi.html"
  }

runBenchmark :: IO ()
runBenchmark = defaultMainWith ftsConfig $ map ftsBench [1000, 10000, 100000]
