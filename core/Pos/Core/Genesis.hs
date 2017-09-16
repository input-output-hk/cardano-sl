module Pos.Core.Genesis
       (
       -- * 'GenesisData'
         mkGenesisData

       -- * Utils
       , balanceDistribution
       , concatAddrDistrs
       , generateGenesisKeyPair
       , generateHdwGenesisSecretKey

       -- * Re-exports
       , module Pos.Core.Genesis.Constants
       , module Pos.Core.Genesis.Generate
       , module Pos.Core.Genesis.Types
       ) where

import           Universum

import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import           Formatting                 (int, sformat, (%))

import           Pos.Binary.Crypto          ()
import           Pos.Core.Coin              (applyCoinPortionUp, divCoin, unsafeMulCoin)
import           Pos.Core.Configuration     (HasConfiguration, HasGenesisBlockVersionData,
                                             generatedGenesisData, genesisAvvmBalances,
                                             genesisBlockVersionData, genesisCertificates,
                                             genesisDelegation, protocolConstants,
                                             sharedSeed)
import           Pos.Core.Types             (Address, BlockVersionData (bvdMpcThd), Coin,
                                             Timestamp)
import           Pos.Crypto.Signing         (PublicKey, SecretKey, deterministicKeyGen)
import           Pos.Crypto.Signing.Safe    (EncryptedSecretKey, emptyPassphrase,
                                             safeDeterministicKeyGen)

-- reexports
import           Pos.Core.Genesis.Canonical ()
import           Pos.Core.Genesis.Constants
import           Pos.Core.Genesis.Generate
import           Pos.Core.Genesis.Types

mkGenesisData :: HasConfiguration => Timestamp -> GenesisData
mkGenesisData startTime =
    GenesisData
    { gdBootStakeholders = ggdBootStakeholders generatedGenesisData
    , gdHeavyDelegation = genesisDelegation
    , gdStartTime = startTime
    , gdVssCerts = genesisCertificates
    , gdNonAvvmBalances = GenesisNonAvvmBalances $
          HM.fromList $ concatAddrDistrs (ggdNonAvvmDistr generatedGenesisData)
    , gdBlockVersionData = genesisBlockVersionData
    , gdProtocolConsts = protocolConstants
    , gdAvvmDistr = genesisAvvmBalances
    , gdFtsSeed = sharedSeed
    }

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- | Given 'BalanceDistribution', calculates a list containing amounts
-- of coins (balances) belonging to genesis addresses.
balanceDistribution :: HasGenesisBlockVersionData => BalanceDistribution -> [Coin]
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
        if richs < applyCoinPortionUp (bvdMpcThd genesisBlockVersionData) total
        then error "Pos.Genesis: RichPoorBalances: richmen balance \
                   \is less than MPC threshold"
        else identity
    basicDist = genericReplicate sdRichmen sdRichBalance ++
                genericReplicate sdPoor sdPoorBalance
balanceDistribution (CustomBalances coins) = coins

-- Converts list of addr distrs to pre-map (addr,coin)
concatAddrDistrs :: HasGenesisBlockVersionData => [AddrDistribution] -> [(Address, Coin)]
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
