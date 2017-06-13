{-| Blockchain genesis. Not to be confused with genesis block in epoch.
    Blockchain genesis means genesis values which are hardcoded in advance
    (before system starts doing anything). Genesis block in epoch exists
    in every epoch and it's not known in advance.
-}

module Pos.Genesis
       (
         module Pos.Core.Genesis
       , module Pos.Ssc.GodTossing.Genesis

       -- * Static state
       , genesisUtxo
       , genesisDelegation
       , genesisSeed
       , accountGenesisIndex
       , wAddressGenesisIndex

       -- * Ssc
       , genesisLeaders
       ) where

import           Universum

import qualified Data.HashMap.Strict        as HM
import           Data.List                  (genericLength, genericReplicate)
import qualified Data.Map.Strict            as M
import           Serokell.Util              (enumerate)

import qualified Pos.Constants              as Const
import           Pos.Core.Types             (Address, StakeholderId)
import           Pos.Crypto                 (EncryptedSecretKey, emptyPassphrase,
                                             firstNonHardened, unsafeHash)
import           Pos.Lrc.FtsPure            (followTheSatoshi, followTheSatoshiUtxo)
import           Pos.Lrc.Genesis            (genesisSeed)
import           Pos.Txp.Core               (TxIn (..), TxOut (..), TxOutAux (..),
                                             TxOutDistribution)
import           Pos.Txp.Toil               (Utxo)
import           Pos.Types                  (Coin, SlotLeaders, applyCoinPortion,
                                             coinToInteger, divCoin, makePubKeyAddress,
                                             mkCoin, unsafeAddCoin, unsafeMulCoin)
import           Pos.Wallet.Web.Util        (deriveLvl2KeyPair)

-- reexports
import           Pos.Core.Genesis
import           Pos.Ssc.GodTossing.Genesis

----------------------------------------------------------------------------
-- Static state
----------------------------------------------------------------------------

-- | First index in derivation path for HD account, which is put to genesis utxo
accountGenesisIndex :: Word32
accountGenesisIndex = firstNonHardened

-- | Second index in derivation path for HD account, which is put to genesis
-- utxo
wAddressGenesisIndex :: Word32
wAddressGenesisIndex = firstNonHardened

-- | Addresses and secret keys of genesis HD wallets' /addresses/.
-- It's important to return 'Address' here, not 'PublicKey', since valid HD
-- wallet address keeps 'HDAddressPayload' attribute which value depends on
-- secret key.
genesisDevHdwAccountKeyDatas :: [(Address, EncryptedSecretKey)]
genesisDevHdwAccountKeyDatas =
    genesisDevHdwSecretKeys <&> \key ->
        fromMaybe (error "Passphrase doesn't match in Genesis") $
        deriveLvl2KeyPair
            emptyPassphrase
            key
            accountGenesisIndex
            wAddressGenesisIndex

genesisDevHdwAccountAddresses :: [Address]
genesisDevHdwAccountAddresses = map fst genesisDevHdwAccountKeyDatas

-- 10000 coins in total. For thresholds testing.
-- 0.5,0.25,0.125,0.0625,0.0312,0.0156,0.0078,0.0039,0.0019,0.0008,0.0006,0.0004,0.0002,0.0001
expTwoDistribution :: [Coin]
expTwoDistribution =
    map mkCoin [5000,2500,1250,625,312,156,78,39,19,8,6,4,2,1]

bitcoinDistribution20 :: [Coin]
bitcoinDistribution20 = map mkCoin
    [200,163,120,105,78,76,57,50,46,31,26,13,11,11,7,4,2,0,0,0]

stakeDistribution :: StakeDistribution -> [(Coin, TxOutDistribution)]
stakeDistribution (FlatStakes stakeholders coins) =
    genericReplicate stakeholders val
  where
    val = (coins `divCoin` stakeholders, [])
stakeDistribution (BitcoinStakes stakeholders coins) =
    map ((, []) . normalize) $ bitcoinDistribution1000Coins stakeholders
  where
    normalize x = x `unsafeMulCoin`
                  coinToInteger (coins `divCoin` (1000 :: Int))
stakeDistribution ExponentialStakes = map (, []) expTwoDistribution
stakeDistribution ts@RichPoorStakes {..} =
    checkMpcThd (getTotalStake ts) sdRichStake $
    map (, []) basicDist
  where
    -- Node won't start if richmen cannot participate in MPC
    checkMpcThd total richs =
        if richs < applyCoinPortion Const.genesisMpcThd total
        then error "Pos.Genesis: RichPoorStakes: richmen stake \
                   \is less than MPC threshold"
        else identity
    basicDist = genericReplicate sdRichmen sdRichStake ++
                genericReplicate sdPoor sdPoorStake
stakeDistribution (ExplicitStakes balances) =
    toList balances
stakeDistribution (CombinedStakes distA distB) =
    stakeDistribution distA <> stakeDistribution distB

bitcoinDistribution1000Coins :: Word -> [Coin]
bitcoinDistribution1000Coins stakeholders
    | stakeholders < 20 = map fst $ stakeDistribution
          (FlatStakes stakeholders (mkCoin 1000))
    | stakeholders == 20 = bitcoinDistribution20
    | otherwise =
        foldl' (bitcoinDistributionImpl ratio) [] $
        enumerate bitcoinDistribution20
  where
    ratio = fromIntegral stakeholders / 20

bitcoinDistributionImpl :: Double -> [Coin] -> (Int, Coin) -> [Coin]
bitcoinDistributionImpl ratio coins (coinIdx, coin) =
    coins ++ toAddValMax : replicate (toAddNum - 1) toAddValMin
  where
    toAddNumMax = ceiling ratio
    toAddNumMin = floor ratio
    toAddNum :: Int
    toAddNum =
        if genericLength coins + realToFrac toAddNumMax >
           realToFrac (coinIdx + 1) * ratio
            then toAddNumMin
            else toAddNumMax
    toAddValMin = coin `divCoin` toAddNum
    toAddValMax = coin `unsafeAddCoin`
                  (toAddValMin `unsafeMulCoin` (toAddNum - 1))

-- | Genesis 'Utxo'.
genesisUtxo :: StakeDistribution -> Utxo
genesisUtxo sd =
    M.fromList $ concat
        [ zipWith zipF (stakeDistribution sd)
            (genesisAddresses <> tailAddresses)
        , map (zipF hwdDistr) hdwAddresses
        ]
  where
    zipF (coin, distr) addr =
        ( TxIn (unsafeHash addr) 0
        , TxOutAux (TxOut addr coin) distr
        )
    tailAddresses = map (makePubKeyAddress . fst .
                         generateGenesisKeyPair)
                      [Const.genesisKeysN ..]
    -- not much money to avoid making wallets slot leaders
    hwdDistr = (mkCoin 100, [])
    -- should be enough for testing.
    genesisDevHdwKeyNum = 2
    hdwAddresses = take genesisDevHdwKeyNum genesisDevHdwAccountAddresses


genesisDelegation :: HashMap StakeholderId [StakeholderId]
genesisDelegation = mempty

----------------------------------------------------------------------------
-- Slot leaders
----------------------------------------------------------------------------

-- | Leaders of genesis. See 'followTheSatoshi'.
genesisLeaders :: Utxo -> SlotLeaders
genesisLeaders genUtxo
    | Const.isDevelopment = followTheSatoshiUtxo genesisSeed genUtxo
    | otherwise = followTheSatoshi genesisSeed $ HM.toList genesisBalances
