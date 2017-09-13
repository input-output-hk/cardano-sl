{-| Blockchain genesis. Not to be confused with genesis block in epoch.
    Blockchain genesis means genesis values which are hardcoded in advance
    (before system starts doing anything). Genesis block in epoch exists
    in every epoch and it's not known in advance.
-}

module Pos.Genesis
       (
       -- * Reexports
         module Pos.Core.Genesis
       , GenesisUtxo(..)

       -- * Context
       , GenesisContext (..)
       , gtcUtxo
       , gtcWStakeholders
       , gtcDelegation

       -- * Static state/functions/common
       , genesisLeaders
       , genesisContextImplicit

       -- * Prod mode genesis
       , genesisContext
       ) where

import           Universum

import           Control.Lens        (at, makeLenses)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import qualified Data.Ratio          as Ratio
import           Formatting          (build, sformat, (%))
import           Serokell.Util       (mapJson)

import           Pos.AllSecrets      (InvAddrSpendingData (unInvAddrSpendingData))
import qualified Pos.Constants       as Const
import           Pos.Core            (AddrSpendingData (PubKeyASD), Address (..), Coin,
                                      GeneratedGenesisData (..), HasConfiguration,
                                      SlotLeaders, StakeholderId, addressHash,
                                      coinToInteger, generatedGenesisData,
                                      makeRedeemAddress, safeExpBalances,
                                      genesisAvvmBalances, sharedSeed,
                                      genesisDelegation)
import           Pos.Crypto          (unsafeHash)
import           Pos.Lrc.FtsPure     (followTheSatoshiUtxo)
import           Pos.Txp.Core        (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil        (GenesisUtxo (..))
import           Pos.Util.Util       (HasLens (..))

-- reexports
import           Pos.Core.Genesis

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

-- | Generates 'GenesisUtxo' given address distributions (which also
-- include stake distributions as parts of addresses).
genesisUtxo :: HasConfiguration => [AddrDistribution] -> GenesisUtxo
genesisUtxo ad = GenesisUtxo . Map.fromList $ map utxoEntry balances
  where
    balances :: HasConfiguration => [(Address, Coin)]
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
genesisContextImplicit :: HasConfiguration => InvAddrSpendingData -> [AddrDistribution] -> GenesisContext
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
generateWStakeholders :: HasConfiguration => InvAddrSpendingData -> AddrDistribution -> GenesisWStakeholders
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
genesisLeaders :: HasConfiguration => GenesisContext -> SlotLeaders
genesisLeaders GenesisContext { _gtcUtxo = (GenesisUtxo utxo)
                              , _gtcWStakeholders = gws
                              } = followTheSatoshiUtxo gws sharedSeed utxo

----------------------------------------------------------------------------
-- Production mode genesis
----------------------------------------------------------------------------

-- | 'GenesisContext' that uses all the data for prod.
genesisContext :: HasConfiguration => GenesisContext
genesisContext =
    let GeneratedGenesisData{..} = generatedGenesisData
        -- 
        addrCoins = HM.toList (getGenesisAvvmBalances genesisAvvmBalances)
        avvmAddrDistr =
            ( map (makeRedeemAddress . fst) addrCoins
            , CustomBalances (map snd addrCoins))
        genesisDistr = avvmAddrDistr : ggdNonAvvmDistr
        genesisUtxoProduction = genesisUtxo genesisDistr
    in  GenesisContext
            { _gtcUtxo = genesisUtxoProduction
            , _gtcWStakeholders = ggdBootStakeholders
            , _gtcDelegation = genesisDelegation
            }
