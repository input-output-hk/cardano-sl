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
       , compileGenData
       , genesisStakeDistribution
       , genesisUtxo
       , genesisDelegation
       , genesisAddresses
       -- ** Genesis data used in development mode
       , genesisDevKeyPairs
       , genesisDevPublicKeys
       , genesisDevSecretKeys

       -- * Ssc
       , genesisLeaders

       -- * Update System
       , genesisBlockVersion
       , genesisBlockVersionData
       , genesisSoftwareVersions
       , genesisScriptVersion
       , genesisSlotDuration
       , genesisMaxBlockSize
       ) where

import           Control.Lens               (_head)
import           Data.Default               (Default (..))
import           Data.List                  (genericLength, genericReplicate)
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import           Data.Time.Units            (Millisecond)
import           Formatting                 (int, sformat, (%))
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util              (enumerate)
import           Universum

import qualified Pos.Constants              as Const
import           Pos.Core.Types             (ScriptVersion, SoftwareVersion (..))
import           Pos.Crypto                 (PublicKey, SecretKey, deterministicKeyGen)
import           Pos.Crypto                 (unsafeHash)
import           Pos.Genesis.Parser         (compileGenData)
import           Pos.Genesis.Types          (GenesisData (..), StakeDistribution (..))
import           Pos.Lrc.FtsPure            (followTheSatoshi)
import           Pos.Txp.Core.Types         (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil.Types         (Utxo)
import           Pos.Types                  (makePubKeyAddress)
import           Pos.Types                  (Address (..), BlockVersion (..), Coin,
                                             SharedSeed (SharedSeed), SlotLeaders,
                                             StakeholderId, applyCoinPortion,
                                             coinToInteger, divCoin, mkCoin,
                                             unsafeAddCoin, unsafeMulCoin)
import           Pos.Update.Core.Types      (BlockVersionData (..))

----------------------------------------------------------------------------
-- Static state
----------------------------------------------------------------------------

-- | List of pairs from 'SecretKey' with corresponding 'PublicKey'.
genesisDevKeyPairs :: [(PublicKey, SecretKey)]
genesisDevKeyPairs = map gen [0 .. Const.genesisN - 1]
  where
    gen :: Int -> (PublicKey, SecretKey)
    gen =
        fromMaybe (error "deterministicKeyGen failed in Genesis") .
        deterministicKeyGen .
        encodeUtf8 .
        T.take 32 . sformat ("My awesome 32-byte seed #" %int % "             ")

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

stakeDistribution :: StakeDistribution -> [Coin]
stakeDistribution (FlatStakes stakeholders coins) =
    genericReplicate stakeholders val
  where
    val = coins `divCoin` stakeholders
stakeDistribution (BitcoinStakes stakeholders coins) =
    map normalize $ bitcoinDistribution1000Coins stakeholders
  where
    normalize x = x `unsafeMulCoin`
                  coinToInteger (coins `divCoin` (1000 :: Int))
stakeDistribution ExponentialStakes = expTwoDistribution
stakeDistribution TestnetStakes {..} =
    map (mkCoin . fromIntegral) $ basicDist & _head %~ (+ rmd)
  where
    -- Total number of richmen
    richs = fromIntegral sdRichmen
    -- Total number of poor
    poors = fromIntegral sdPoor
    -- Minimum amount of money to become rich
    thresholdRich = coinToInteger $
        applyCoinPortion Const.genesisMpcThd sdTotalStake
    -- Maximal amount of total money which poor stakeholders can hold
    maxPoorStake = (thresholdRich - 1) * poors
    -- Minimum amount of richmen's money to prevent poors becoming richmen
    minRichStake = coinToInteger sdTotalStake - maxPoorStake
    -- Minimum amount of money per richman to maintain number of richmen
    minRich = minRichStake `div` richs
    -- Final amount of money per richman
    rich = max thresholdRich minRich
    -- Amount of money left to poor
    poorStake = coinToInteger sdTotalStake - richs * rich
    -- Money per poor and modulo (it goes to first richman)
    (poor, rmd) = if poors == 0
                  then (0, poorStake)
                  else (poorStake `div` poors, poorStake `mod` poors)
    -- Coin distribution (w/o modulo added)
    basicDist = genericReplicate richs rich ++ genericReplicate poors poor
stakeDistribution (ExplicitStakes balances) =
    toList balances

bitcoinDistribution1000Coins :: Word -> [Coin]
bitcoinDistribution1000Coins stakeholders
    | stakeholders < 20 = stakeDistribution
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
    M.fromList . zipWith zipF (stakeDistribution sd) $ genesisAddresses
  where
    zipF coin addr =
        ( TxIn (unsafeHash addr) 0
        , (TxOutAux {toaOut = TxOut addr coin, toaDistr = []}))

genesisDelegation :: HashMap StakeholderId [StakeholderId]
genesisDelegation = mempty

----------------------------------------------------------------------------
-- Slot leaders
----------------------------------------------------------------------------

genesisSeed :: SharedSeed
genesisSeed = SharedSeed "vasa opasa skovoroda Ggurda boroda provoda"

-- | Leaders of genesis. See 'followTheSatoshi'.
genesisLeaders :: Utxo -> SlotLeaders
genesisLeaders = followTheSatoshi genesisSeed

----------------------------------------------------------------------------
-- Update system
----------------------------------------------------------------------------

-- | BlockVersion used at the very beginning.
genesisBlockVersion :: BlockVersion
genesisBlockVersion =
    BlockVersion
    { bvMajor = 0
    , bvMinor = 0
    , bvAlt = 0
    }

-- | Software Versions
genesisSoftwareVersions :: [SoftwareVersion]
genesisSoftwareVersions = [Const.curSoftwareVersion { svNumber = 0 }]

-- | 'BlockVersionData' for genesis 'BlockVersion'.
genesisBlockVersionData :: BlockVersionData
genesisBlockVersionData =
    BlockVersionData
    { bvdScriptVersion = genesisScriptVersion
    , bvdSlotDuration = Const.genesisSlotDuration
    , bvdMaxBlockSize = Const.genesisMaxBlockSize
    , bvdMaxHeaderSize = Const.genesisMaxHeaderSize
    , bvdMaxTxSize = Const.genesisMaxTxSize
    , bvdMaxProposalSize = Const.genesisMaxUpdateProposalSize
    , bvdMpcThd = Const.genesisMpcThd
    , bvdHeavyDelThd = Const.genesisHeavyDelThd
    , bvdUpdateVoteThd = Const.genesisUpdateVoteThd
    , bvdUpdateProposalThd = Const.genesisUpdateProposalThd
    , bvdUpdateImplicit = Const.genesisUpdateImplicit
    , bvdUpdateSoftforkThd = Const.genesisUpdateSoftforkThd
    }

-- | ScriptVersion used at the very beginning
genesisScriptVersion :: ScriptVersion
genesisScriptVersion = 0

-- | Initial slot duration
genesisSlotDuration :: Millisecond
genesisSlotDuration = Const.genesisSlotDuration

-- | Initial block size limit
genesisMaxBlockSize :: Byte
genesisMaxBlockSize = Const.genesisMaxBlockSize
