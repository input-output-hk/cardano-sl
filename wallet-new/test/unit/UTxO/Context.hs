{-# LANGUAGE DeriveAnyClass #-}

-- | Context needed for the translation between DSL and Cardano types
module UTxO.Context (
    -- * Cardano provided context
    CardanoContext(..)
  , initCardanoContext
    -- * Actors
  , Actors(..)
  , Rich(..)
  , Poor(..)
  , Stakeholder(..)
  , initActors
    -- * Mapping between addresses
  , ActorIx(..)
  , AddrIx
  , Addr(..)
  , AddrMap(..)
  , initAddrMap
    -- * Our custom context
  , Context(..)
  , initContext
    -- * Derived information
  , resolveAddr
  , resolveAddress
  , isKnownAddress
  , leaderForSlot
    -- ** Block sign info
  , BlockSignInfo(..)
  , blockSignInfo
  , blockSignInfoForSlot
  ) where

import Universum
import Formatting (sformat, bprint, build, (%))
import Serokell.Util (listJson, pairF)
import Text.Show.Pretty (PrettyVal)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty  as NE
import qualified Data.Map.Strict     as Map
import qualified Data.Text.Buildable

import Pos.Context
import Pos.Core
import Pos.Crypto
import Pos.Lrc.Genesis
import Pos.Txp

import UTxO.Crypto

{-------------------------------------------------------------------------------
  Summary of the information we get about the genesis block from Cardano core
-------------------------------------------------------------------------------}

-- | The information returned by core
data CardanoContext = CardanoContext {
      ccLeaders  :: SlotLeaders
    , ccStakes   :: StakesMap
    , ccBlock0   :: GenesisBlock
    , ccData     :: GenesisData
    , ccUtxo     :: Utxo
    , ccSecrets  :: GeneratedSecrets

      -- | Initial balances
      --
      -- Derived from 'ccUtxo'.
    , ccBalances :: [(Address, Coin)]

      -- | Hash of block0
      --
      -- NOTE: Derived from 'ccBlock0', /not/ the same as 'genesisHash'.
    , ccHash0 :: HeaderHash
    }

initCardanoContext :: HasConfiguration => CardanoContext
initCardanoContext = CardanoContext{..}
  where
    ccLeaders  = genesisLeaders
    ccStakes   = genesisStakes
    ccBlock0   = genesisBlock0
    ccData     = genesisData
    ccUtxo     = unGenesisUtxo genesisUtxo
    ccSecrets  = fromMaybe (error "initCardanoContext: secrets unavailable") $
                 generatedSecrets

    ccBalances = utxoToAddressCoinPairs ccUtxo
    ccHash0    = (blockHeaderHash . Left . _gbHeader) ccBlock0

{-------------------------------------------------------------------------------
  More explicit representation of the various actors in the genesis block

  When heavy-weight delegation is enabled, 'generateGenesisData' creates three
  sets of actors, each with their own set of secret keys:

  * The 'poor' actors, with a small balance ('gsPoorSecrets')
    (these use HD addresses)
  * The 'rich' actors, with a large balance ('gsRichSecrets')
    (these do not use HD addresses)
  * The stakeholders ('gsDlgIssuersSecrets')
    (no addresses get generated for these)

  (Using the Ouroboros-neutral word "actor" intentionally to avoid confusion.)

  All addresses (for the poor and rich actors) use 'BootstrapEraDistr' as their
  stake distribution attribute; in 'bootstrapEraDistr' this is interpreted as
  a distribution over the 'gdBootStakeholders' in the genesis data, which in
  turn is derived from 'gsDlgIssuersSecrets' in 'generateGenesisData'.

  Additionally, 'generateGenesisData' generates a set of 'ProxySKHeavy' (aka
  'ProxySecretKey EpochIndex') delegating from the stakeholders (the
  'pskIssuerPk') to the rich actors (the 'pskDelegatePk'). The following excerpt
  from Section 8.2, Delegation Schema, of the Ouroboros paper is relevant here:

  > A stakeholder can transfer the right to generate blocks by creating a proxy
  > signing key that allows the delegate to sign messages of the form (st, d,
  > slj) (i.e., the format of messages signed in Protocol πDPoS to authenticate
  > a block).

  So it's actually the rich actors that sign blocks on behalf of the
  stakeholders.

  The genesis UTxO computed by 'genesisUtxo', being a Utxo, is simply a set of
  unspent transaction outputs; 'genesisStakes' then uses 'utxoToStakes' to turn
  this into a 'StakeMap'. A key component of this transaction is 'txOutStake',
  which relies on 'bootstrapEtaDistr' for addresses marked 'BootstrapEraDistr'.
  Thus the 'StakesMap' computed by 'genesisStakes' will contain 'StakeholderId's
  of the stakeholders, even though (somewhat confusingly) the stakeholders are
  never actually assigned any addresses.

  Finally, 'genesisLeaders' uses 'followTheSatoshiUtxo' applied to the
  'genesisUtxo' to compute the 'SlotLeaders' (aka 'NonEmpty StakeholderId').
  Since the stake holders have delegated their signing privilege to the rich
  actors, however, it is actually the rich actors that sign the blocks. The
  mapping from the (public keys) of the stakeholders to the (public keys) of the
  rich actors is recorded in 'gdHeavyDelegation' of 'GenesisData'.

  Concretely, the generated genesis data looks something like this:

  > Context{
  >     actors:   Actors{
  >       rich:  [ R1 .. R4 ]
  >     , poor:  [ P1 .. P12 ]
  >     , stake: [ S1 .. S4 ]
  >   }
  >   , leaders:  [ S3, ... ]
  >   , stakes:   [
  >     , (S1, 11250000000000016 coin(s))
  >     , (S2, 11249999999999992 coin(s))
  >     , (S3, 11249999999999992 coin(s))
  >     , (S4, 11249999999999992 coin(s))
  >     ------------------------
  >     total: 44999999999999992
  >   ]
  >   , balances: [
  >       ... "fake AVVM" balances omitted ...
  >       ( P1_1, 37499999999166 coin(s))
  >     , ( P2_1, 37499999999166 coin(s))
  >     , ( P3_1, 37499999999166 coin(s))
  >     , ( P4_1, 37499999999166 coin(s))
  >     , ( P5_1, 37499999999166 coin(s))
  >     , ( P6_1, 37499999999166 coin(s))
  >     , ( P7_1, 37499999999166 coin(s))
  >     , ( P8_1, 37499999999166 coin(s))
  >     , ( P9_1, 37499999999166 coin(s))
  >     , (P10_1, 37499999999166 coin(s))
  >     , (P11_1, 37499999999166 coin(s))
  >     , (P12_1, 37499999999166 coin(s))
  >     , (R1, 11137499999752500 coin(s))
  >     , (R2, 11137499999752500 coin(s))
  >     , (R3, 11137499999752500 coin(s))
  >     , (R4, 11137499999752500 coin(s))
  >     ------------------------
  >     total: 44999999999999992
  >   ]
  > }

  where there is a delegation from each of the stakeholders to one of the
  rich actors.

  NOTE: It is somewhat odd that the stakeholders delegate " back " to the rich
  actors. In reality this wouldn't happen, and instead there would be a fourth
  set of actors, with no stake nor any balance, whose sole role is to sign
  blocks. This means that if their keys get compromised, the actual stakeholders
  can then delegate to a new set and the keys of the stakeholders themselves
  never need to be online.
-------------------------------------------------------------------------------}

-- | Actors in the translation context
data Actors = Actors {
      actorsRich  :: Map PublicKey Rich
    , actorsPoor  :: Map PublicKey Poor
    , actorsStake :: Map StakeholderId Stakeholder
    }
  deriving (Show)

-- | A rich actor has a key and a "simple" (non-HD) address
data Rich = Rich {
      richKey  :: KeyPair
    , richAddr :: Address
    }
  deriving (Show)

-- | A poor actor gets a HD wallet, so it has a SecretKey per address
-- (current generation just creates a single address though)
--
-- NOTE: `encToSecret :: EncryptedSecretKey -> SecretKey`
data Poor = Poor {
      poorKey   :: EncKeyPair
    , poorAddrs :: [(EncKeyPair, Address)]
    }
  deriving (Show)

data Stakeholder = Stakeholder {
      stkKey :: KeyPair
    , stkDel :: DelegatedTo Rich
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Deriving 'Actors' from 'CardanoContext'

  TODO: This derivation is more complicated than it ought to be. The mapping
  from the secret keys to the corresponding addresses is already present in
  generateGenesisData, but it is not returned. I see no choice currently but to
  recompute it. This is unfortunate because it means that when
  'generateGenesisData' changes, we'll be out of sync here. Also, we're assuming
  here that 'tboUseHDAddresses' is true ('useHDAddresses' must be set to true in
  the config yaml file).
-------------------------------------------------------------------------------}

-- | Compute generated actors
initActors :: CardanoContext -> Actors
initActors CardanoContext{..} = Actors{..}
  where
    actorsRich  :: Map PublicKey Rich
    actorsPoor  :: Map PublicKey Poor
    actorsStake :: Map StakeholderId Stakeholder

    actorsRich  = Map.fromList $ map mkRich  $ gsRichSecrets       ccSecrets
    actorsPoor  = Map.fromList $ map mkPoor  $ gsPoorSecrets       ccSecrets
    actorsStake = Map.fromList $ map mkStake $ gsDlgIssuersSecrets ccSecrets

    mkRich :: RichSecrets -> (PublicKey, Rich)
    mkRich RichSecrets{..} = (kpPub richKey, Rich {..})
      where
        richKey :: KeyPair
        richKey = keyPair rsPrimaryKey

        richAddr :: Address
        richAddr = makePubKeyAddressBoot (toPublic rsPrimaryKey)

    mkPoor :: EncryptedSecretKey -> (PublicKey, Poor)
    mkPoor poorSec = (ekpPub poorKey, Poor {..})
      where
        poorKey :: EncKeyPair
        poorKey = encKeyPair poorSec

        poorAddrs :: [(EncKeyPair, Address)]
        poorAddrs = [ case deriveFirstHDAddress
                             (IsBootstrapEraAddr True)
                             emptyPassphrase
                             poorSec of
                        Nothing          -> error "impossible"
                        Just (addr, key) -> (encKeyPair key, addr)
                    ]

    mkStake :: SecretKey -> (StakeholderId, Stakeholder)
    mkStake stkSec = (kpHash stkKey, Stakeholder{..})
      where
        stkKey :: KeyPair
        stkKey = keyPair stkSec

        stkDel :: DelegatedTo Rich
        stkDel = DelegatedTo{..}

        delTo :: Rich
        delTo = Map.findWithDefault
                     (error ("initActors: delegate not found"))
                     (pskDelegatePk delPSK)
                     actorsRich

        delPSK :: ProxySKHeavy
        delPSK = HM.lookupDefault
                   (error ("initActors: issuer not found"))
                   (kpHash stkKey)
                   (unGenesisDelegation $ gdHeavyDelegation ccData)

{-------------------------------------------------------------------------------
  In many cases the difference between rich and poor actors is not important
-------------------------------------------------------------------------------}

-- | Index the actors by number
data ActorIx = IxRich Int | IxPoor Int
  deriving (Show, Eq, Ord, Generic, PrettyVal)

-- | Address index of a regular actor
--
-- We don't track the difference between rich and poor actors at the type
-- level to make things a bit more uniform.
type AddrIx = Int

-- | Address is given by an actor index and an address index
data Addr = Addr {
      addrActorIx :: ActorIx
    , addrIx      :: AddrIx
    }
  deriving (Show, Eq, Ord, Generic, PrettyVal)

-- | Mapping between our addresses and Cardano addresses
data AddrMap = AddrMap {
      addrMap    :: Map Addr (KeyPair, Address)
    , addrRevMap :: Map Address Addr
    }

-- | Compute initial address mapping
initAddrMap :: Actors -> AddrMap
initAddrMap Actors{..} = AddrMap{
      addrMap    = Map.fromList rawMap
    , addrRevMap = Map.fromList $ map (swap . second snd) rawMap
    }
  where
    rawMap :: [(Addr, (KeyPair, Address))]
    rawMap = map       richRawAddr  (zip [0..] $ Map.elems actorsRich)
          ++ concatMap poorRawAddrs (zip [0..] $ Map.elems actorsPoor)

    richRawAddr :: (Int, Rich) -> (Addr, (KeyPair, Address))
    richRawAddr (actorIx, Rich{..}) = (
          Addr (IxRich actorIx) 0
        , (richKey, richAddr)
        )

    poorRawAddrs :: (Int, Poor) -> [(Addr, (KeyPair, Address))]
    poorRawAddrs (actorIx, Poor{..}) = map poorRawAddr $ zip [0..] poorAddrs
      where
        poorRawAddr :: (Int, (EncKeyPair, Address))
                    -> (Addr, (KeyPair, Address))
        poorRawAddr (addrIx, (ekp, addr)) = (
              Addr (IxPoor actorIx) addrIx
            , (fromEncKeyPair ekp, addr)
            )

{-
actorWithIx :: ActorIx -> Actors -> RegularActor
actorWithIx (IxRich i) Actors{..} = ActorRich $ Map.elems actorsRich !! i
actorWithIx (IxPoor i) Actors{..} = ActorPoor $ Map.elems actorsPoor !! i


-- | Rich or poor actr, but not a stakeholder
data RegularActor = ActorRich Rich | ActorPoor Poor

actorAddr :: AddrIx -> RegularActor -> (KeyPair, Address)
actorAddr ix (ActorRich Rich{..}) =
    if ix == 0
      then (richKey, richAddr)
      else error "actorAddr: rich actors have single address"
actorAddr ix (ActorPoor Poor{..}) =
    if ix < length poorAddrs
      then first fromEncKeyPair (poorAddrs !! ix)
      else error "actorAddr: address index out of bounds"


resolveAddr :: Addr -> Actors -> (KeyPair, Address)
resolveAddr (actorIx, addrIx) = actorAddr addrIx . actorWithIx actorIx
-}

{-------------------------------------------------------------------------------
  Translation context

  The environment we need to be able to do the translation between the DSL
  and Cardano types. Accumulation of the environments above.
-------------------------------------------------------------------------------}

data Context = Context {
      tcCardano  :: CardanoContext
    , tcActors   :: Actors
    , tcAddrMap  :: AddrMap
    }

initContext :: CardanoContext -> Context
initContext tcCardano = Context{..}
  where
    tcActors  = initActors  tcCardano
    tcAddrMap = initAddrMap tcActors

{-------------------------------------------------------------------------------
  Derived information
-------------------------------------------------------------------------------}

resolveAddr :: Addr -> Context -> (KeyPair, Address)
resolveAddr addr Context{..} =
    fromMaybe
      (error $ sformat ("resolveAddr: " % build % " not found") addr)
      (Map.lookup addr addrMap)
  where
    AddrMap{..} = tcAddrMap

resolveAddress :: Address -> Context -> Addr
resolveAddress addr Context{..} =
    fromMaybe
      (error $ sformat ("resolveAddress: " % build % " not found") addr)
      (Map.lookup addr addrRevMap)
  where
    AddrMap{..} = tcAddrMap

-- | Check if something is a known address
--
-- NOTE: We don't translate all addresses in the generated genesis block to
-- actors; in particular, we ignore AVVM accounts. For these addresses
-- 'isKnownAddress' will return @False@.
isKnownAddress :: Address -> Context -> Bool
isKnownAddress addr Context{..} = Map.member addr addrRevMap
  where
    AddrMap{..} = tcAddrMap

leaderForSlot :: SlotId -> Context -> Stakeholder
leaderForSlot slotId Context{..} = actorsStake Map.! leader
  where
    Actors{..}         = tcActors
    CardanoContext{..} = tcCardano

    leader :: StakeholderId
    leader = ccLeaders NE.!! slotIx

    slotIx :: Int
    slotIx = fromIntegral $ getSlotIndex (siSlot slotId)

{-------------------------------------------------------------------------------
  Derive block sign info from a 'Stakeholder'
-------------------------------------------------------------------------------}

-- | Information needed to sign a block
data BlockSignInfo = BlockSignInfo {
      bsiLeader :: PublicKey    -- ^ Real slot leader
    , bsiKey    :: SecretKey    -- ^ Secret key of the actor signing it
    , bsiPSK    :: ProxySKHeavy -- ^ Prove that the actor may sign the block
    }

-- | 'BlockSignInfo' can be derived from the slot's 'Stakeholder'
blockSignInfo :: Stakeholder -> BlockSignInfo
blockSignInfo Stakeholder{..} = BlockSignInfo{..}
  where
    DelegatedTo{..} = stkDel
    Rich{..}        = delTo

    bsiLeader = kpPub stkKey
    bsiKey    = kpSec richKey
    bsiPSK    = delPSK

blockSignInfoForSlot :: SlotId -> Context -> BlockSignInfo
blockSignInfoForSlot slotId = blockSignInfo . leaderForSlot slotId

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable Rich where
  build Rich{..} = bprint
      ( "Rich"
      % "{ key:  " % build
      % ", addr: " % build
      % "}"
      )
      richKey
      richAddr

instance Buildable Poor where
  build Poor{..} = bprint
      ( "Poor"
      % "{ key:   " % build
      % ", addrs: " % listJson
      % "}"
      )
      poorKey
      (map (bprint pairF) poorAddrs)

instance Buildable Stakeholder where
  build Stakeholder{..} = bprint
      ( "Stakeholder"
      % "{ key: " % build
      % ", del: " % build
      % "}"
      )
      stkKey
      stkDel

instance Buildable Actors where
  build Actors{..} = bprint
      ( "Actors"
      % "{ rich:  " % listJson
      % ", poor:  " % listJson
      % ", stake: " % listJson
      % "}"
      )
      (Map.elems actorsRich)
      (Map.elems actorsPoor)
      (Map.elems actorsStake)

instance Buildable ActorIx where
  build (IxRich ix) = bprint ("Rich " % build) ix
  build (IxPoor ix) = bprint ("Poor " % build) ix

instance Buildable Addr where
  build Addr{..} = bprint
      ( "Addr"
      % "{ actorIx: " % build
      % ", addrIx:  " % build
      % "}"
      )
      addrActorIx
      addrIx

-- | We don't show the whole thing, this is for debugging primarily
instance Buildable CardanoContext where
  build CardanoContext{..} = bprint
      ( "CardanoContext"
      % "{ leaders:  " % listJson
      % ", stakes:   " % listJson
      % ", balances: " % listJson
      % "}"
      )
      ccLeaders
      (map (bprint pairF) (HM.toList ccStakes))
      (map (bprint pairF) ccBalances)

instance Buildable Context where
  build Context{..} = bprint
      ( "Context"
      % "{ cardano: " % build
      % ", actors:  " % build
      % "}"
      )
      tcCardano
      tcActors
