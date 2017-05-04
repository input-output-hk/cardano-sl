{-| Blockchain genesis. Not to be confused with genesis block in epoch.
    Blockchain genesis means genesis values which are hardcoded in advance
    (before system starts doing anything). Genesis block in epoch exists
    in every epoch and it's not known in advance.
-}

module Pos.Genesis
       (
       -- * Static state
         StakeDistribution (..)
       , GenesisData (..)
       , getTotalStake
       , compileGenData
       , genesisStakeDistribution
       , genesisUtxo
       , genesisDelegation
       , genesisAddresses
       , genesisSeed
       , genesisBalances
       , walletGenesisIndex
       , accountGenesisIndex
       -- ** Genesis data used in development mode
       , genesisDevKeyPairs
       , genesisDevPublicKeys
       , genesisDevSecretKeys
       , generateGenesisBackupPhrase
       , genesisDevHdwSecretKeys

       -- * Ssc
       , genesisLeaders
       ) where

import           Control.Lens          (ix)
import           Data.Default          (Default (..))
import           Data.List             (genericLength, genericReplicate)
import qualified Data.Map.Strict       as M
import qualified Data.Text             as T
import           Formatting            (int, sformat, stext, (%))
import           Serokell.Util         (enumerate)
import           Universum

import qualified Pos.Constants         as Const
import           Pos.Core.Types        (StakeholderId)
import           Pos.Crypto            (EncryptedSecretKey, PublicKey, SecretKey,
                                        deterministicKeyGen, emptyPassphrase, encToPublic,
                                        unsafeHash)
import           Pos.Genesis.Parser    (compileGenData)
import           Pos.Genesis.Types     (GenesisData (..), StakeDistribution (..),
                                        getTotalStake)
import           Pos.Lrc.FtsPure       (followTheSatoshi)
import           Pos.Lrc.Genesis       (genesisSeed)
import           Pos.Txp.Core.Types    (TxIn (..), TxOut (..), TxOutAux (..),
                                        TxOutDistribution)
import           Pos.Txp.Toil.Types    (Utxo)
import           Pos.Types             (Address (..), Coin, SlotLeaders, applyCoinPortion,
                                        coinToInteger, divCoin, makePubKeyAddress, mkCoin,
                                        unsafeAddCoin, unsafeMulCoin)
import           Pos.Util.BackupPhrase (BackupPhrase, mkBackupPhrase9, safeKeysFromPhrase)
import           Pos.Util.Mnemonics    as Mnemonics
import           Pos.Wallet.Web.Util   (deriveLvl2KeyPair)

----------------------------------------------------------------------------
-- Static state
----------------------------------------------------------------------------

generateGenesisKeyPair :: Int -> (PublicKey, SecretKey)
generateGenesisKeyPair =
    fromMaybe (error "deterministicKeyGen failed in Genesis") .
    deterministicKeyGen .
    encodeUtf8 .
    T.take 32 . sformat ("My awesome 32-byte seed #" %int % "             ")

-- | List of pairs from 'SecretKey' with corresponding 'PublicKey'.
genesisDevKeyPairs :: [(PublicKey, SecretKey)]
genesisDevKeyPairs = map generateGenesisKeyPair [0 .. Const.genesisN - 1]

-- | List of 'PublicKey's in genesis.
genesisDevPublicKeys :: [PublicKey]
genesisDevPublicKeys = map fst genesisDevKeyPairs

-- | List of 'SecretKey's in genesis.
genesisDevSecretKeys :: [SecretKey]
genesisDevSecretKeys = map snd genesisDevKeyPairs

-- | List of addresses in genesis. See 'genesisPublicKeys'.
genesisAddresses :: [Address]
genesisAddresses
    | Const.isDevelopment = map makePubKeyAddress genesisDevPublicKeys
    | otherwise           = gdAddresses compileGenData

genesisStakeDistribution :: StakeDistribution
genesisStakeDistribution
    | Const.isDevelopment = def
    | otherwise           = gdDistribution compileGenData

genesisBalances :: HashMap StakeholderId Coin
genesisBalances
    | Const.isDevelopment = mempty
    | otherwise           = gdBootstrapBalances compileGenData

generateGenesisBackupPhrase :: Int -> BackupPhrase
generateGenesisBackupPhrase i =
    let unique = fromMaybe (error "Mnemonics dictionary it too small for Genesis")
               $ Mnemonics.wl ^? ix i
    in mkBackupPhrase9
        [ "avocado"
        , "avoid"
        , "awake"
        , "aware"
        , "away"
        , "awesome"
        , "awful"
        , "awkward"
        , toText unique
        ]

generateHdwGenesisSecretKey :: Int -> EncryptedSecretKey
generateHdwGenesisSecretKey =
    fst .
    either failed identity .
    safeKeysFromPhrase emptyPassphrase .
    generateGenesisBackupPhrase
  where
    failed = error . sformat ("safeKeysFromPhrase failed in Genesis: "%stext)

-- | List of 'SecretKey's in genesis for HD wallets.
genesisDevHdwSecretKeys :: [EncryptedSecretKey]
genesisDevHdwSecretKeys =
    map generateHdwGenesisSecretKey [0 .. Const.genesisN - 1]

-- | First index in derivation path for HD account, which is put to genesis utxo
walletGenesisIndex :: Word32
walletGenesisIndex = 0

-- | Second index in derivation path for HD account, which is put to genesis utxo
accountGenesisIndex :: Word32
accountGenesisIndex = 0

genesisDevHdwAccountSecretKeys :: [EncryptedSecretKey]
genesisDevHdwAccountSecretKeys =
    genesisDevHdwSecretKeys <&> \key ->
        snd $
        deriveLvl2KeyPair
            emptyPassphrase
            key
            walletGenesisIndex
            accountGenesisIndex

instance Default StakeDistribution where
    def = FlatStakes Const.genesisN
              (mkCoin 10000 `unsafeMulCoin` (Const.genesisN :: Int))

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
    M.fromList . zipWith zipF (stakeDistribution sd) $
    genesisAddresses <> allTailAddresses
  where
    zipF (coin, distr) addr =
        ( TxIn (unsafeHash addr) 0
        , TxOutAux (TxOut addr coin) distr
        )
    tailAddresses = map (makePubKeyAddress . fst . generateGenesisKeyPair)
        [Const.genesisN ..]
    tailHdwAddresses = makePubKeyAddress . encToPublic <$>
        genesisDevHdwAccountSecretKeys
    allTailAddresses =
        concat $ zipWith (\a b -> [a, b]) tailAddresses tailHdwAddresses

genesisDelegation :: HashMap StakeholderId [StakeholderId]
genesisDelegation = mempty

----------------------------------------------------------------------------
-- Slot leaders
----------------------------------------------------------------------------

-- | Leaders of genesis. See 'followTheSatoshi'.
genesisLeaders :: Utxo -> SlotLeaders
genesisLeaders = followTheSatoshi genesisSeed
