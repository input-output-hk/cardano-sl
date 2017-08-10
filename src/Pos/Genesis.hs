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
       , generateWStakeholders
       , genesisContextImplicit

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

import           Control.Lens               (at, makeLenses)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (genericLength, genericReplicate)
import qualified Data.Map.Strict            as M
import qualified Data.Ratio                 as Ratio
import           Ether.Internal             (HasLens (..))
import           Formatting                 (sformat)
import           Serokell.Util              (enumerate, listJson, pairF)

import           Pos.AllSecrets             (InvAddrSpendingData (unInvAddrSpendingData),
                                             mkInvAddrSpendingData)
import qualified Pos.Constants              as Const
import           Pos.Core                   (AddrSpendingData (PubKeyASD), Address (..),
                                             Coin, SlotLeaders, addressHash,
                                             applyCoinPortionUp, coinToInteger,
                                             deriveLvl2KeyPair, divCoin,
                                             makePubKeyAddress, mkCoin, safeExpStakes,
                                             unsafeAddCoin, unsafeMulCoin)
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
stakeDistribution (ExponentialStakes n mc) =
    reverse $ take (fromIntegral n) $
    iterate (`unsafeMulCoin` (2::Integer)) mc
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

-- Converts list of addr distrs to pre-map (addr,coin)
concatAddrDistrs :: [AddrDistribution] -> [(Address, Coin)]
concatAddrDistrs addrDistrs =
    concatMap (uncurry zip . second stakeDistribution) addrDistrs

-- | Generates genesis 'Utxo' given weighted boot stakeholders and
-- address distributions. All the stake is distributed among genesis
-- stakeholders (using 'genesisSplitBoot').
genesisUtxo :: GenesisWStakeholders -> [AddrDistribution] -> GenesisUtxo
genesisUtxo gws@(GenesisWStakeholders bootStakeholders) ad
    | null bootStakeholders =
        error "genesisUtxo: no stakeholders for the bootstrap era"
    | otherwise = GenesisUtxo . M.fromList $ map utxoEntry balances
  where
    balances :: [(Address, Coin)]
    balances = concatAddrDistrs ad
    utxoEntry (addr, coin) =
        ( TxIn (unsafeHash addr) 0
        , TxOutAux (TxOut addr coin) (outDistr coin))
    outDistr = genesisSplitBoot gws

-- | Same as 'genesisUtxo' but generates 'GenesisWStakeholders' set
-- using 'generateWStakeholders' inside and wraps it all in
-- 'GenesisContext'.
genesisContextImplicit :: InvAddrSpendingData -> [AddrDistribution] -> GenesisContext
genesisContextImplicit invAddrSpendingData addrDistr =
    GenesisContext utxo genStakeholders
  where
    genStakeholders = generateWStakeholders invAddrSpendingData addrDistr
    utxo = genesisUtxo genStakeholders addrDistr

-- | Generate weighted stakeholders using passed address distribution.
generateWStakeholders :: InvAddrSpendingData -> [AddrDistribution] -> GenesisWStakeholders
generateWStakeholders (unInvAddrSpendingData -> addrToSpending) addrDistrs =
    if null withCoins
        then GenesisWStakeholders mempty
        else GenesisWStakeholders $ foldr step mempty withCoins
  where
    withCoins = concatAddrDistrs addrDistrs
    coins = map snd withCoins
    intCoins = map coinToInteger coins
    commonGcd = foldr1 gcd intCoins
    targetTotalWeight = maxBound @Word16 -- for the maximal precision
    safeConvert :: Integer -> Word16
    safeConvert i
        | i <= 0 =
            error $
            "generateWStakeholders can't convert: non-positive coin " <> show i
        | i > fromIntegral targetTotalWeight =
            error $
            "generateWStakeholders can't convert: too big " <> show i <>
            ", withCoins: " <>
            sformat listJson (map (sformat pairF) withCoins)
        | otherwise = fromIntegral i
    calcWeight :: Coin -> Word16
    calcWeight balance =
        safeConvert $ floor $ (coinToInteger balance) Ratio.% (commonGcd)
    step (addr, balance) =
        case addrToSpending ^. at addr of
            Just (PubKeyASD (addressHash -> sId)) ->
                HM.insertWith (+) sId (calcWeight balance)
            _ -> identity

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
devStakesDistr Nothing Nothing Nothing (Just n) = safeExpStakes n
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
    distrSize = length $ stakeDistribution distr
    tailPks = map (fst . generateGenesisKeyPair) [Const.genesisKeysN ..]
    mainPks = genesisDevPublicKeys <> tailPks
    mainAddrs = take distrSize $ map makePubKeyAddress mainPks
    mainSpendingDataList = map PubKeyASD mainPks
    invAddrSpendingData =
        mkInvAddrSpendingData $ mainAddrs `zip` mainSpendingDataList
    aDistr =
        [ (mainAddrs, distr) -- Addresses from passed stake
        , (hdwAddresses, hdwDistr) -- HDW addresses for testing
        ]

    -- Genesis stakeholders
    gws :: GenesisWStakeholders
    gws = generateWStakeholders invAddrSpendingData [(mainAddrs, distr)]

    -- HD wallets
    hdwSize = 2 -- should be positive
    -- 200 coins split among hdwSize users. Should be small sum enough
    -- to avoid making wallets slot leaders.
    hdwDistr = FlatStakes (fromIntegral hdwSize) (mkCoin 200)
    -- should be enough for testing.
    hdwAddresses = take hdwSize genesisDevHdwAccountAddresses
    genesisDevHdwAccountAddresses :: [Address]
    genesisDevHdwAccountAddresses = map fst genesisDevHdwAccountKeyDatas
