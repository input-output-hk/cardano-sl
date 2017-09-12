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
       , gtcDelegation

       -- * Static state/functions/common
       , balanceDistribution
       , genesisLeaders
       , genesisContextImplicit

       -- * Prod mode genesis
       , genesisContextProductionM

       -- * Dev mode genesis
       , devBalancesDistr
       , devGenesisContext
       , concatAddrDistrs

       ) where

import           Universum

import           Control.Lens               (at, makeLenses)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (genericReplicate)
import qualified Data.Map.Strict            as Map
import qualified Data.Ratio                 as Ratio
import           Formatting                 (build, sformat, (%))
import           Serokell.Util              (mapJson)
import           System.Wlog                (WithLogger)

import           Pos.AllSecrets             (InvAddrSpendingData (unInvAddrSpendingData),
                                             mkInvAddrSpendingData)
import qualified Pos.Constants              as Const
import           Pos.Core                   (AddrSpendingData (PubKeyASD), Address (..),
                                             Coin, HasCoreConstants,
                                             IsBootstrapEraAddr (..), SlotLeaders,
                                             StakeholderId, addressHash,
                                             applyCoinPortionUp, coinToInteger,
                                             deriveLvl2KeyPair, divCoin,
                                             makePubKeyAddressBoot, makeRedeemAddress,
                                             mkCoin, safeExpBalances, unsafeMulCoin)
import           Pos.Crypto                 (EncryptedSecretKey, emptyPassphrase,
                                             firstHardened, unsafeHash)
import           Pos.Lrc.FtsPure            (followTheSatoshiUtxo)
import           Pos.Lrc.Genesis            (genesisSeed)
import           Pos.Testnet                (genTestnetOrMainnetData)
import           Pos.Txp.Core               (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil               (GenesisUtxo (..))
import           Pos.Util.Util              (HasLens (..))

-- reexports
import           Pos.Core.Genesis
import           Pos.Ssc.GodTossing.Genesis

----------------------------------------------------------------------------
-- Context
----------------------------------------------------------------------------

-- | Genesis context consists configurable parts of genesis state.
--
-- TODO: probably 'gtc' prefix should be changed to 'gc'.
data GenesisContext = GenesisContext
    { _gtcUtxo          :: !GenesisUtxo
      -- ^ Genesis utxo.
    , _gtcWStakeholders :: !GenesisWStakeholders
      -- ^ Weighted genesis stakeholders.
    , _gtcDelegation    :: !GenesisDelegation
      -- ^ Genesis state of heavyweight delegation.
    } deriving (Show)

makeLenses ''GenesisContext

instance HasLens GenesisUtxo GenesisContext GenesisUtxo where
    lensOf = gtcUtxo

instance HasLens GenesisWStakeholders GenesisContext GenesisWStakeholders where
    lensOf = gtcWStakeholders

instance HasLens GenesisDelegation GenesisContext GenesisDelegation where
    lensOf = gtcDelegation

----------------------------------------------------------------------------
-- Static state & funcitons
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
        if richs < applyCoinPortionUp Const.genesisMpcThd total
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

-- | Generates 'GenesisUtxo' given address distributions (which also
-- include stake distributions as parts of addresses).
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
--
-- It uses empty genesis delegation, because non-empty one is useful
-- only in production and for production we have
-- 'genesisContextProduction'.
genesisContextImplicit :: InvAddrSpendingData -> [AddrDistribution] -> GenesisContext
genesisContextImplicit _ [] = error "genesisContextImplicit: empty list passed"
genesisContextImplicit invAddrSpendingData addrDistr =
    GenesisContext utxo genStakeholders noGenesisDelegation
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
        FlatBalances _ _    ->
            createList $ map ((,1) . toStakeholderId) addrs
        RichPoorBalances{..} ->
            createList $ map ((,1) . toStakeholderId) $
            take (fromIntegral sdRichmen) addrs
        e@(ExponentialBalances _ _) ->
            GenesisWStakeholders $
            assignWeights iasd $ addrs `zip` balanceDistribution e
        CustomBalances coins ->
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
genesisContextProductionM
    :: (MonadIO m, MonadThrow m, WithLogger m)
    => m GenesisContext
genesisContextProductionM = do
    (testnetDistr, bootStakeholders, _) <-
        genTestnetOrMainnetData genesisProdInitializer
    let addrCoins = HM.toList (getGenesisAvvmBalances genesisProdAvvmBalances)
    let avvmAddrDistr =
            ( map (makeRedeemAddress . fst) addrCoins
            , CustomBalances (map snd addrCoins))
    let genesisDistr = avvmAddrDistr : testnetDistr
    let genesisUtxoProduction = genesisUtxo genesisDistr
    pure $
        GenesisContext
        { _gtcUtxo = genesisUtxoProduction
        , _gtcWStakeholders = bootStakeholders
        , _gtcDelegation = genesisProdDelegation
        }

----------------------------------------------------------------------------
-- Development mode genesis
----------------------------------------------------------------------------

-- | Chooses among common distributions for dev mode.
devBalancesDistr
    :: Maybe (Int, Int)                   -- flat distr
    -> Maybe (Int, Int, Integer, Double)  -- rich/poor distr
    -> Maybe Int                          -- exp distr
    -> BalanceDistribution
devBalancesDistr Nothing Nothing Nothing = genesisDevFlatDistr
devBalancesDistr (Just (nodes, coins)) Nothing Nothing =
    FlatBalances (fromIntegral nodes) (mkCoin (fromIntegral coins))
devBalancesDistr Nothing (Just (richs, poors, coins, richShare)) Nothing =
    checkConsistency $ RichPoorBalances {..}
  where
    sdRichmen = fromIntegral richs
    sdPoor = fromIntegral poors

    totalRichBalance = round $ richShare * fromIntegral coins
    totalPoorBalance = coins - totalRichBalance
    richBalance = totalRichBalance `div` fromIntegral richs
    poorBalance = totalPoorBalance `div` fromIntegral poors
    sdRichBalance = mkCoin $ fromIntegral richBalance
    sdPoorBalance = mkCoin $ fromIntegral poorBalance

    checkConsistency =
        if poorBalance <= 0 || richBalance <= 0
        then error "Impossible to make RichPoorBalances with given parameters."
        else identity
devBalancesDistr Nothing Nothing (Just n) = safeExpBalances n
devBalancesDistr _ _ _ =
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
            firstHardened
            firstHardened

-- | 'GenesisContext' for dev mode. It's supposed that you pass the
-- distribution from 'devBalancesDistr' here. This function will add dev
-- genesis addresses and hd addrs/distr.
--
-- Related genesis stakeholders are computed using only related
-- distribution passed, hd keys have no stake in boot era.
--
-- Genesis delegation is empty. Non-empty one can be supported, but
-- probably will never be needed.
devGenesisContext :: BalanceDistribution -> GenesisContext
devGenesisContext distr =
    GenesisContext (genesisUtxo aDistr) gws noGenesisDelegation
  where
    distrSize = length $ balanceDistribution distr
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
    -- 20 ADA (20 millon coins) split among hdwSize users.
    -- Shouldn't mess with LRC after CSL-1502
    hdwDistr = FlatBalances (fromIntegral hdwSize) (mkCoin 20000000)
    -- should be enough for testing.
    hdwAddresses = take hdwSize genesisDevHdwAccountAddresses
    genesisDevHdwAccountAddresses :: [Address]
    genesisDevHdwAccountAddresses = map fst genesisDevHdwAccountKeyDatas
