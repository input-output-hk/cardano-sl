module Pos.Core.Genesis
       (
       -- * Data from 'GenesisSpec'
         genesisAvvmBalances
       , genesisInitializer
       , genesisDelegation

       , generatedGenesisData
       , genesisSecretKeys
       , genesisHdwSecretKeys
       , genesisVssSecretKeys
       , genesisCertificates

       -- * 'GenesisData'
       , mkGenesisData

       -- * Utils
       , balanceDistribution
       , concatAddrDistrs
       , generateGenesisKeyPair
       , generateHdwGenesisSecretKey

       -- * Re-exports
       , module Pos.Core.Genesis.Constants
       , module Pos.Core.Genesis.Parser
       , module Pos.Core.Genesis.Generate
       , module Pos.Core.Genesis.Types
       ) where

import           Universum

import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import           Formatting                 (int, sformat, (%))

import           Pos.Binary.Crypto          ()
import           Pos.Core.Coin              (applyCoinPortionUp, divCoin, unsafeMulCoin)
import           Pos.Core.Types             (Address, Coin, Timestamp)
import           Pos.Core.Vss               (VssCertificatesMap)
import           Pos.Crypto.SafeSigning     (EncryptedSecretKey, emptyPassphrase,
                                             safeDeterministicKeyGen)
import           Pos.Crypto.SecretSharing   (VssKeyPair)
import           Pos.Crypto.Signing         (PublicKey, SecretKey, deterministicKeyGen)

-- reexports
import           Pos.Core.Genesis.Canonical ()
import           Pos.Core.Genesis.Constants
import           Pos.Core.Genesis.Generate
import           Pos.Core.Genesis.Parser
import           Pos.Core.Genesis.Types

----------------------------------------------------------------------------
-- Data taken from 'GenesisSpec'
----------------------------------------------------------------------------

-- | Genesis avvm balances.
genesisAvvmBalances :: GenesisAvvmBalances
genesisAvvmBalances = gsAvvmDistr genesisSpec

-- | Genesis initializer determines way of initialization
-- utxo, bootstrap stakeholders, etc.
genesisInitializer :: GenesisInitializer
genesisInitializer = gsInitializer genesisSpec

-- | 'GenesisDelegation' for production mode.
genesisDelegation :: GenesisDelegation
genesisDelegation = gsHeavyDelegation genesisSpec

generatedGenesisData :: GeneratedGenesisData
generatedGenesisData = generateGenesisData genesisInitializer

genesisCertificates :: VssCertificatesMap
genesisCertificates = ggdGtData generatedGenesisData

genesisSecretKeys :: Maybe [SecretKey]
genesisSecretKeys = map (view _1) <$> ggdSecretKeys generatedGenesisData

genesisHdwSecretKeys :: Maybe [EncryptedSecretKey]
genesisHdwSecretKeys = map (view _2) <$> ggdSecretKeys generatedGenesisData

genesisVssSecretKeys :: Maybe [VssKeyPair]
genesisVssSecretKeys = map (view _3) <$> ggdSecretKeys generatedGenesisData

mkGenesisData :: Timestamp -> GenesisData
mkGenesisData startTime =
    GenesisData
    { gdBootStakeholders = ggdBootStakeholders generatedGenesisData
    , gdHeavyDelegation = genesisDelegation
    , gdStartTime = startTime
    , gdVssCerts = genesisCertificates
    , gdNonAvvmBalances =
          HM.fromList $ concatAddrDistrs (ggdNonAvvmDistr generatedGenesisData)
    , gdBlockVersionData = genesisBlockVersionData
    , gdProtocolConsts = genesisProtocolConstants
    , gdAvvmDistr = gsAvvmDistr genesisSpec
    , gdFtsSeed = gsFtsSeed genesisSpec
    }

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- | Given 'BalanceDistribution', calculates a list containing amounts
-- of coins (balances) belonging to genesis addresses.
balanceDistribution :: BalanceDistribution -> [Coin]
balanceDistribution (FlatBalances stakeholders coins) =
    genericReplicate stakeholders val
  where
    val = coins `divCoin` stakeholders
balanceDistribution (ExponentialBalances n mc) =
    reverse $ take (fromIntegral n) $
    iterate (`unsafeMulCoin` (2::Integer)) mc
balanceDistribution ts@RichPoorBalances {..} =
    checkMpcThd (getTotalBalance ts) sdRichBalance basicDist
  where
    -- Node won't start if richmen cannot participate in MPC
    checkMpcThd total richs =
        if richs < applyCoinPortionUp genesisMpcThd total
        then error "Pos.Genesis: RichPoorBalances: richmen balance \
                   \is less than MPC threshold"
        else identity
    basicDist = genericReplicate sdRichmen sdRichBalance ++
                genericReplicate sdPoor sdPoorBalance
balanceDistribution (CustomBalances coins) = coins

-- Converts list of addr distrs to pre-map (addr,coin)
concatAddrDistrs :: [AddrDistribution] -> [(Address, Coin)]
concatAddrDistrs addrDistrs =
    concatMap (uncurry zip . second balanceDistribution) addrDistrs

generateGenesisKeyPair :: Int -> (PublicKey, SecretKey)
generateGenesisKeyPair =
    deterministicKeyGen .
    encodeUtf8 .
    T.take 32 . sformat ("My awesome 32-byte seed #" %int % "             ")

generateHdwGenesisSecretKey :: Int -> EncryptedSecretKey
generateHdwGenesisSecretKey =
    snd .
    flip safeDeterministicKeyGen emptyPassphrase .
    encodeUtf8 .
    T.take 32 . sformat ("My 32-byte hdw seed #" %int % "                  ")
