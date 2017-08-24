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
       , genesisLeaders
       , genesisContextImplicit

       -- * Prod mode genesis
       , genesisContextProduction

       -- * Dev mode genesis
       , accountGenesisIndex
       , wAddressGenesisIndex
       , devStakesDistr
       , devGenesisContext
       , concatAddrDistrs

       ) where

import           Universum

import           Control.Lens               (at, makeLenses)
import           Data.List                  (genericReplicate)
import qualified Data.Map.Strict            as Map
import qualified Data.Ratio                 as Ratio
import           Ether.Internal             (HasLens (..))
import           Formatting                 (build, sformat, (%))
import           Serokell.Util              (mapJson)

import           Pos.AllSecrets             (InvAddrSpendingData (unInvAddrSpendingData),
                                             mkInvAddrSpendingData)
import qualified Pos.Constants              as Const
import           Pos.Core                   (AddrSpendingData (PubKeyASD), Address (..),
                                             Coin, HasCoreConstants,
                                             IsBootstrapEraAddr (..), SlotLeaders,
                                             StakeholderId, addressHash,
                                             applyCoinPortionUp, coinToInteger,
                                             deriveLvl2KeyPair, divCoin,
                                             makePubKeyAddressBoot, mkCoin, safeExpStakes,
                                             unsafeMulCoin)
import           Pos.Crypto                 (EncryptedSecretKey, emptyPassphrase,
                                             firstHardened, unsafeHash)
import           Pos.Lrc.FtsPure            (followTheSatoshiUtxo)
import           Pos.Lrc.Genesis            (genesisSeed)
import           Pos.Txp.Core               (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil               (GenesisUtxo (..))

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

-- | Given 'StakeDistribution', calculates a list containing amounts
-- of coins (balances) belonging to genesis addresses.
stakeDistribution :: StakeDistribution -> [Coin]
stakeDistribution (FlatStakes stakeholders coins) =
    genericReplicate stakeholders val
  where
    val = coins `divCoin` stakeholders
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
genesisUtxo :: [AddrDistribution] -> GenesisUtxo
genesisUtxo ad = GenesisUtxo . Map.fromList $ map utxoEntry balances
  where
    balances :: [(Address, Coin)]
    balances = concatAddrDistrs ad
    utxoEntry (addr, coin) =
        ( TxInUtxo (unsafeHash addr) 0
        , TxOutAux (TxOut addr coin)
        )

-- | Same as 'genesisUtxo' but generates 'GenesisWStakeholders' set
-- using 'generateWStakeholders' inside and wraps it all in
-- 'GenesisContext'.
genesisContextImplicit :: InvAddrSpendingData -> [AddrDistribution] -> GenesisContext
genesisContextImplicit _ [] = error "genesisContextImplicit: empty list passed"
genesisContextImplicit invAddrSpendingData addrDistr =
    GenesisContext utxo genStakeholders
  where
    mergeStakeholders :: Map StakeholderId Word16
                      -> Map StakeholderId Word16
                      -> Map StakeholderId Word16
    mergeStakeholders =
        Map.unionWithKey $ \_ a b ->
        error $ "genesisContextImplicit: distributions have " <>
                "common keys which is forbidden " <>
                pretty a <> ", " <> pretty b
    genStakeholders =
        GenesisWStakeholders $
        foldr1 mergeStakeholders $
        map (getGenesisWStakeholders .
             generateWStakeholders invAddrSpendingData) addrDistr
    utxo = genesisUtxo addrDistr

-- | Generate weighted stakeholders using passed address distribution.
generateWStakeholders :: InvAddrSpendingData -> AddrDistribution -> GenesisWStakeholders
generateWStakeholders iasd@(unInvAddrSpendingData -> addrToSpending) (addrs,stakeDistr) =
    case stakeDistr of
        FlatStakes _ _    ->
            createList $ map ((,1) . toStakeholderId) addrs
        RichPoorStakes{..} ->
            createList $ map ((,1) . toStakeholderId) $
            take (fromIntegral sdRichmen) addrs
        e@(ExponentialStakes _ _) ->
            GenesisWStakeholders $
            assignWeights iasd $ addrs `zip` stakeDistribution e
        CustomStakes coins ->
            GenesisWStakeholders $ assignWeights iasd $ addrs `zip` coins
  where
    createList = GenesisWStakeholders . Map.fromList
    toStakeholderId addr = case addrToSpending ^. at addr of
        Just (PubKeyASD (addressHash -> sId)) -> sId
        _ -> error $ sformat ("generateWStakeholders: "%build%
                              " is not a pubkey addr or not in the map") addr

assignWeights :: InvAddrSpendingData -> [(Address,Coin)] -> Map StakeholderId Word16
assignWeights (unInvAddrSpendingData -> addrToSpending) withCoins =
    foldr step mempty withCoins
  where
    coins = map snd withCoins
    intCoins = map coinToInteger coins
    commonGcd = foldr1 gcd intCoins
    targetTotalWeight = maxBound @Word16 -- for the maximal precision
    safeConvert :: Integer -> Word16
    safeConvert i
        | i <= 0 =
          error $ "generateWStakeholders can't convert: non-positive coin " <> show i
        | i > fromIntegral targetTotalWeight =
          error $ "generateWStakeholders can't convert: too big " <> show i <>
                  ", withCoins: " <> sformat mapJson withCoins
        | otherwise = fromIntegral i
    calcWeight :: Coin -> Word16
    calcWeight balance =
        safeConvert $ floor $
        (coinToInteger balance) Ratio.%
        (commonGcd)
    step (addr, balance) =
        case addrToSpending ^. at addr of
            Just (PubKeyASD (addressHash -> sId)) ->
                Map.insertWith (+) sId (calcWeight balance)
            _ -> identity

-- | Compute leaders of the 0-th epoch from stake distribution.
genesisLeaders :: HasCoreConstants => GenesisContext -> SlotLeaders
genesisLeaders GenesisContext { _gtcUtxo = (GenesisUtxo utxo)
                              , _gtcWStakeholders = gws
                              } = followTheSatoshiUtxo gws genesisSeed utxo

----------------------------------------------------------------------------
-- Production mode genesis
----------------------------------------------------------------------------

-- | 'GenesisContext' that uses all the data for prod.
genesisContextProduction :: GenesisContext
genesisContextProduction =
    GenesisContext genesisUtxoProduction genesisProdBootStakeholders
  where
    -- 'GenesisUtxo' used in production.
    genesisUtxoProduction :: GenesisUtxo
    genesisUtxoProduction = genesisUtxo genesisProdAddrDistribution

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
    -> Maybe (Int, Int, Integer, Double)  -- rich/poor distr
    -> Maybe Int                          -- exp distr
    -> StakeDistribution
devStakesDistr Nothing Nothing Nothing = genesisDevFlatDistr
devStakesDistr (Just (nodes, coins)) Nothing Nothing =
    FlatStakes (fromIntegral nodes) (mkCoin (fromIntegral coins))
devStakesDistr Nothing (Just (richs, poors, coins, richShare)) Nothing =
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
devStakesDistr Nothing Nothing (Just n) = safeExpStakes n
devStakesDistr _ _ _ =
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
            (IsBootstrapEraAddr True)
            emptyPassphrase
            key
            accountGenesisIndex
            wAddressGenesisIndex

-- | 'GenesisContext' for dev mode. It's supposed that you pass the
-- distribution from 'devStakesDistr' here. This function will add dev
-- genesis addresses and hd addrs/distr.
--
-- Related genesis stakeholders are computed using only related
-- distribution passed, hd keys have no stake in boot era.
devGenesisContext :: StakeDistribution -> GenesisContext
devGenesisContext distr =
    GenesisContext (genesisUtxo aDistr) gws
  where
    distrSize = length $ stakeDistribution distr
    tailPks = map (fst . generateGenesisKeyPair) [Const.genesisKeysN ..]
    mainPks = genesisDevPublicKeys <> tailPks
    mainAddrs = take distrSize $ map makePubKeyAddressBoot mainPks
    mainSpendingDataList = map PubKeyASD mainPks
    invAddrSpendingData =
        mkInvAddrSpendingData $ mainAddrs `zip` mainSpendingDataList

    mainADistr = (mainAddrs, distr) -- Addresses from passed stake
    aDistr = [ mainADistr
             , (hdwAddresses, hdwDistr)  -- HDW addresses for testing
             ]

    -- Genesis stakeholders
    gws :: GenesisWStakeholders
    gws = generateWStakeholders invAddrSpendingData mainADistr

    -- HD wallets
    hdwSize = 2 -- should be positive
    -- 200 coins split among hdwSize users. Should be small sum enough
    -- to avoid making wallets slot leaders.
    hdwDistr = FlatStakes (fromIntegral hdwSize) (mkCoin 200)
    -- should be enough for testing.
    hdwAddresses = take hdwSize genesisDevHdwAccountAddresses
    genesisDevHdwAccountAddresses :: [Address]
    genesisDevHdwAccountAddresses = map fst genesisDevHdwAccountKeyDatas
