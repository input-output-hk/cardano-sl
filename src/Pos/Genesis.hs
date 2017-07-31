{-| Blockchain genesis. Not to be confused with genesis block in epoch.
    Blockchain genesis means genesis values which are hardcoded in advance
    (before system starts doing anything). Genesis block in epoch exists
    in every epoch and it's not known in advance.
-}

module Pos.Genesis
       (
       -- * Reexports
         module Pos.Core.Genesis
       , module Pos.Ssc.GodTossing.Genesis
       , GenesisUtxo(..)

       -- * Context
       , GenesisContext (..)
       , gtcUtxo
       , gtcWStakeholders

       -- * Static state/functions/common
       , stakeDistribution
       , genesisUtxo
       , genesisSeed
       , genesisLeaders

       -- * Prod mode genesis
       , genesisUtxoProduction
       , genesisContextProduction

       -- * Dev mode genesis
       , accountGenesisIndex
       , wAddressGenesisIndex
       , devStakesDistr
       , devAddrDistr

       ) where

import           Universum

import           Control.Lens               (makeLenses)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (genericLength, genericReplicate)
import qualified Data.Map.Strict            as M
import           Ether.Internal             (HasLens (..))
import           Serokell.Util              (enumerate)

import qualified Pos.Constants              as Const
import           Pos.Core                   (Address (..), Coin, SlotLeaders, addressHash,
                                             applyCoinPortionUp, coinToInteger,
                                             deriveLvl2KeyPair, divCoin,
                                             makePubKeyAddress, mkCoin, unsafeAddCoin,
                                             unsafeGetCoin, unsafeMulCoin)
import           Pos.Crypto                 (EncryptedSecretKey, emptyPassphrase,
                                             firstHardened, unsafeHash)
import           Pos.Lrc.FtsPure            (followTheSatoshi)
import           Pos.Lrc.Genesis            (genesisSeed)
import           Pos.Txp.Core               (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil               (GenesisUtxo (..), utxoToStakes)

-- reexports
import           Pos.Core.Genesis
import           Pos.Ssc.GodTossing.Genesis

----------------------------------------------------------------------------
-- Context
----------------------------------------------------------------------------

-- | Genesis context related to transaction processing.
data GenesisContext = GenesisContext
    { _gtcUtxo          :: !GenesisUtxo
      -- ^ Genesis utxo.
    , _gtcWStakeholders :: !GenesisWStakeholders
      -- ^ Weighted genesis stakeholders.
    } deriving (Show)

makeLenses ''GenesisContext

instance HasLens GenesisUtxo GenesisContext GenesisUtxo where
    lensOf = gtcUtxo

instance HasLens GenesisWStakeholders GenesisContext GenesisWStakeholders where
    lensOf = gtcWStakeholders

----------------------------------------------------------------------------
-- Static state & funcitons
----------------------------------------------------------------------------

bitcoinDistribution20 :: [Coin]
bitcoinDistribution20 = map mkCoin
    [200,163,120,105,78,76,57,50,46,31,26,13,11,11,7,4,2,0,0,0]

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

-- | Given 'StakeDistribution', calculates a list containing amounts
-- of coins (balances) belonging to genesis addresses.
stakeDistribution :: StakeDistribution -> [Coin]
stakeDistribution (FlatStakes stakeholders coins) =
    genericReplicate stakeholders val
  where
    val = coins `divCoin` stakeholders
stakeDistribution (BitcoinStakes stakeholders coins) =
    map normalize $ bitcoinDistribution1000Coins stakeholders
  where
    normalize x =
        x `unsafeMulCoin` coinToInteger (coins `divCoin` (1000 :: Int))
stakeDistribution (ExponentialStakes n (fromIntegral . unsafeGetCoin -> mc)) =
    reverse $ map mkCoin $ take (fromIntegral n) $ iterate (*2) mc
stakeDistribution ts@RichPoorStakes {..} =
    checkMpcThd (getTotalStake ts) sdRichStake basicDist
  where
    -- Node won't start if richmen cannot participate in MPC
    checkMpcThd total richs =
        if richs < applyCoinPortionUp Const.genesisMpcThd total
        then error "Pos.Genesis: RichPoorStakes: richmen stake \
                   \is less than MPC threshold"
        else identity
    basicDist = genericReplicate sdRichmen sdRichStake ++
                genericReplicate sdPoor sdPoorStake
stakeDistribution (CustomStakes coins) = coins

-- | Generates genesis 'Utxo' given weighted boot stakeholders and
--  address distributions. All the stake is distributed among genesis
--  stakeholders (using 'genesisSplitBoot').
genesisUtxo ::
       GenesisWStakeholders -> [AddrDistribution] -> GenesisUtxo
genesisUtxo gws@(GenesisWStakeholders bootStakeholders) ad
    | null bootStakeholders =
        error "genesisUtxo: no stakeholders for the bootstrap era"
    | otherwise = GenesisUtxo . M.fromList $ map utxoEntry balances
  where
    -- This type is drunk.
    somethingComplicated :: [([Address], [Coin])]
    somethingComplicated = map (second stakeDistribution) ad
    balances :: [(Address, Coin)]
    balances = concatMap (uncurry zip) somethingComplicated
    utxoEntry (addr, coin) =
        ( TxIn (unsafeHash addr) 0
        , TxOutAux (TxOut addr coin) (outDistr coin))
    -- Empty distribution for PubKey address means that the owner of
    -- this address will have the stake.
    genesisSplitBoot' x c =
        either (\e -> error $ "genesisUtxo can't split: " <> show e <>
                              ", genesis utxo " <> show ad)
               identity
               (genesisSplitBoot x c)
    outDistr = genesisSplitBoot' gws

-- | Compute leaders of the 0-th epoch from stake distribution.
genesisLeaders :: GenesisUtxo -> SlotLeaders
genesisLeaders (GenesisUtxo utxo) =
    followTheSatoshi genesisSeed $ HM.toList $ utxoToStakes utxo

----------------------------------------------------------------------------
-- Production mode genesis
----------------------------------------------------------------------------

-- | 'GenesisUtxo' used in production.
genesisUtxoProduction :: GenesisUtxo
genesisUtxoProduction =
    genesisUtxo genesisProdBootStakeholders genesisProdAddrDistribution

-- | 'GenesisContext' that uses all the data for prod.
genesisContextProduction :: GenesisContext
genesisContextProduction =
    GenesisContext genesisUtxoProduction genesisProdBootStakeholders

----------------------------------------------------------------------------
-- Development mode genesis
----------------------------------------------------------------------------

-- | First index in derivation path for HD account, which is put to genesis utxo
accountGenesisIndex :: Word32
accountGenesisIndex = firstHardened

-- | Second index in derivation path for HD account, which is put to genesis
-- utxo
wAddressGenesisIndex :: Word32
wAddressGenesisIndex = firstHardened

-- | Chooses among common distributions for dev mode.
devStakesDistr
    :: Maybe (Int, Int)                   -- flat distr
    -> Maybe (Int, Int)                   -- bitcoin distr
    -> Maybe (Int, Int, Integer, Double)  -- rich/poor distr
    -> Maybe Int                          -- exp distr
    -> StakeDistribution
devStakesDistr Nothing Nothing Nothing Nothing = genesisDevFlatDistr
devStakesDistr (Just (nodes, coins)) Nothing Nothing Nothing =
    FlatStakes (fromIntegral nodes) (mkCoin (fromIntegral coins))
devStakesDistr Nothing (Just (nodes, coins)) Nothing Nothing =
    BitcoinStakes (fromIntegral nodes) (mkCoin (fromIntegral coins))
devStakesDistr Nothing Nothing (Just (richs, poors, coins, richShare)) Nothing =
    checkConsistency $ RichPoorStakes {..}
  where
    sdRichmen = fromIntegral richs
    sdPoor = fromIntegral poors

    totalRichStake = round $ richShare * fromIntegral coins
    totalPoorStake = coins - totalRichStake
    richStake = totalRichStake `div` fromIntegral richs
    poorStake = totalPoorStake `div` fromIntegral poors
    sdRichStake = mkCoin $ fromIntegral richStake
    sdPoorStake = mkCoin $ fromIntegral poorStake

    checkConsistency =
        if poorStake <= 0 || richStake <= 0
        then error "Impossible to make RichPoorStakes with given parameters."
        else identity
devStakesDistr Nothing Nothing Nothing (Just n) =
    ExponentialStakes (fromIntegral n) (mkCoin 0)
devStakesDistr _ _ _ _ =
    error "Conflicting distribution options were enabled. \
          \Choose one at most or nothing."

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

-- | Address distribution for dev mode. It's supposed that you pass
-- the distribution from 'devStakesDistr' here. This function will add
-- dev genesis addresses and hd addrs/distr.
devAddrDistr :: StakeDistribution -> ([AddrDistribution], GenesisWStakeholders)
devAddrDistr distr = (aDistr, gws)
  where
    gws = GenesisWStakeholders $ HM.fromList $
          map (first addressHash) $ take distrSize genesisDevPublicKeys `zip` [1..]
    aDistr = [ (mainAddrs, distr)        -- Addresses from passed stake
             , (hdwAddresses, hdwDistr)  -- HDW addresses for testing
             ]
    distrSize = length $ stakeDistribution distr
    mainAddrs =
        take distrSize $ genesisDevAddresses <> tailAddresses
    tailAddresses =
        map (makePubKeyAddress . fst . generateGenesisKeyPair)
            [Const.genesisKeysN ..]
    hdwSize = 2 -- should be positive
    -- 200 coins split among hdwSize users. Should be small sum enough
    -- to avoid making wallets slot leaders.
    hdwDistr = FlatStakes (fromIntegral hdwSize) (mkCoin 200)
    -- should be enough for testing.
    hdwAddresses = take hdwSize genesisDevHdwAccountAddresses

    genesisDevHdwAccountAddresses :: [Address]
    genesisDevHdwAccountAddresses = map fst genesisDevHdwAccountKeyDatas
