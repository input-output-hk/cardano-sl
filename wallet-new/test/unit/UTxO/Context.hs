{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
  , Avvm(..)
  , initActors
    -- * Mapping between addresses
  , ActorIx(..)
  , AddrIx
  , Addr(..)
  , maxAddrSize
  , isAvvmAddr
  , isPoorAddr
  , AddrInfo(..)
  , AddrMap(..)
  , initAddrMap
    -- * Our custom context
  , TransCtxt(..)
  , initContext
    -- * Derived information
  , resolveAddr
  , resolveAddress
  , leaderForSlot
    -- ** Block sign info
  , BlockSignInfo(..)
  , blockSignInfo
  , blockSignInfoForSlot
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Formatting (bprint, build, sformat, (%))
import qualified Formatting.Buildable
import           Serokell.Util (listJson, mapJson, pairF)
import           Serokell.Util.Base16 (base16F)
import           Universum

import           Pos.Chain.Lrc
import           Pos.Chain.Txp
import           Pos.Core
import           Pos.Core.Block (BlockHeader (..), GenesisBlock, HeaderHash,
                     blockHeaderHash, genesisBlock0, _gbHeader)
import           Pos.Core.Delegation (ProxySKHeavy)
import           Pos.Core.Genesis (GeneratedSecrets (..), GenesisData (..),
                     GenesisDelegation (..), PoorSecret (..), RichSecrets (..))
import           Pos.Crypto

import           UTxO.Crypto

{-------------------------------------------------------------------------------
  Summary of the information we get about the genesis block from Cardano core
-------------------------------------------------------------------------------}

-- | The information returned by core
data CardanoContext = CardanoContext {
      ccStakes      :: StakesMap
    , ccBlock0      :: GenesisBlock
    , ccData        :: GenesisData
    , ccUtxo        :: Utxo
    , ccSecrets     :: GeneratedSecrets
    , ccMagic       :: ProtocolMagic

      -- | Initial stake distribution
    , ccInitLeaders :: SlotLeaders

      -- | Initial balances
      --
      -- Derived from 'ccUtxo'.
    , ccBalances    :: [(Address, Coin)]

      -- | Hash of block0
      --
      -- NOTE: Derived from 'ccBlock0', /not/ the same as 'genesisHash'.
    , ccHash0       :: HeaderHash

      -- | Number of slots in an epoch
    , ccEpochSlots  :: SlotCount
    }

initCardanoContext :: HasConfiguration => ProtocolMagic -> CardanoContext
initCardanoContext ccMagic = CardanoContext{..}
  where
    ccLeaders     = genesisLeaders epochSlots
    ccStakes      = genesisStakes
    ccBlock0      = genesisBlock0 ccMagic (GenesisHash genesisHash) ccLeaders
    ccData        = genesisData
    ccUtxo        = unGenesisUtxo genesisUtxo
    ccSecrets     = fromMaybe (error "initCardanoContext: no secrets") $
                      generatedSecrets
    ccInitLeaders = ccLeaders
    ccBalances    = utxoToAddressCoinPairs ccUtxo
    ccHash0       = (blockHeaderHash . BlockHeaderGenesis . _gbHeader) ccBlock0
    ccEpochSlots  = epochSlots

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
  * A set of AVVM accounts, with a small balance ('gsFakeAvvmSeeds')

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

  In order to compute the stake distribution, `txOutStake` needs a series of
  weights for each of the stakeholders. In the test configuration these
  are all set to 1

  > 1a1ff7035103d8a9: 1
  > 281e5ae9e357970a: 1
  > 44283ce5c44e6e00: 1
  > 5f53e01e1366aeda: 1

  Finally, 'genesisLeaders' uses 'followTheSatoshiUtxo' applied to the
  'genesisUtxo' to compute the 'SlotLeaders' (aka 'NonEmpty StakeholderId').
  Since the stake holders have delegated their signing privilege to the rich
  actors, however, it is actually the rich actors that sign the blocks. The
  mapping from the (public keys) of the stakeholders to the (public keys) of the
  rich actors is recorded in 'gdHeavyDelegation' of 'GenesisData'.

  Concretely, the generated genesis data looks something like this:

  > TransCtxt{
  >     cardano: CardanoContext{
  >       leaders:  [ S3, S4, S1, S1, S3, S2, S2, S1, S4, S4, .. ]
  >     , stakes:   [
  >         (S3, 11249999999999992 coin(s))
  >       , (S1, 11250000000000016 coin(s))
  >       , (S2, 11249999999999992 coin(s))
  >       , (S4, 11249999999999992 coin(s))
  >     ]
  >     , balances: [
  >           (AVVM1, 100000 coin(s))
  >         , (AVVM2, 100000 coin(s))
  >         , (AVVM3, 100000 coin(s))
  >         , (AVVM4, 100000 coin(s))
  >         , (AVVM5, 100000 coin(s))
  >         , (AVVM6, 100000 coin(s))
  >         , (AVVM7, 100000 coin(s))
  >         , (AVVM8, 100000 coin(s))
  >         , (AVVM9, 100000 coin(s))
  >         , (AVVM10, 100000 coin(s))
  >         , (P01_1, 37499999999166 coin(s))
  >         , (P02_1, 37499999999166 coin(s))
  >         , (P03_1, 37499999999166 coin(s))
  >         , (P04_1, 37499999999166 coin(s))
  >         , (P05_1, 37499999999166 coin(s))
  >         , (P06_1, 37499999999166 coin(s))
  >         , (P07_1, 37499999999166 coin(s))
  >         , (P08_1, 37499999999166 coin(s))
  >         , (P09_1, 37499999999166 coin(s))
  >         , (P10_1, 37499999999166 coin(s))
  >         , (P11_1, 37499999999166 coin(s))
  >         , (P12_1, 37499999999166 coin(s))
  >         , (R1, 11137499999752500 coin(s))
  >         , (R2, 11137499999752500 coin(s))
  >         , (R3, 11137499999752500 coin(s)P04_1
  >         , (R4, 11137499999752500 coin(s))
  >     ]
  >   }
  >   , actors:  Actors{
  >       rich:  [
  >         Rich{ .., addr: R1 }
  >         ..
  >       , Rich{ .., addr: R4 }
  >     ]
  >     , poor:  [
  >         Poor{ ..,, addrs: [ (.., P01_1) ] }
  >         ..
  >       , Poor{ ..,, addrs: [ (.., P12_1) ] }
  >     ]
  >     , stake: [
  >         Stakeholder{ key: RegularKeyPair{ .., hash: S1 } , del: DelegatedTo{ to:  Rich{ .., addr: R1 }, psk: ProxySk { w = epoch #0, ..  } } }
  >       , Stakeholder{ key: RegularKeyPair{ .., hash: S2 } , del: DelegatedTo{ to:  Rich{ .., addr: R3 }, psk: ProxySk { w = epoch #0, ..  } } }
  >       , Stakeholder{ key: RegularKeyPair{ .., hash: S3 } , del: DelegatedTo{ to:  Rich{ .., addr: R4 }, psk: ProxySk { w = epoch #0, ..  } } }
  >       , Stakeholder{ key: RegularKeyPair{ .., hash: S4 } , del: DelegatedTo{ to:  Rich{ .., addr: R2 }, psk: ProxySk { w = epoch #0, ..  } } }
  >     ]
  >     , avvm:  [
  >         Avvm{ .., addr: AVVM1 }
  >         ..
  >       , Avvm{ .., addr: AVVM10 }
  >     ]
  >   }
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
    , actorsAvvm  :: Map RedeemPublicKey Avvm
    }
  deriving (Show)

-- | A rich actor has a key and a "simple" (non-HD) address
data Rich = Rich {
      richKey  :: RegularKeyPair
    , richAddr :: Address
    }
  deriving (Show)

-- | A poor actor gets a HD wallet, so it has a keypair per address
-- (current generation just creates a single address though)
data Poor = Poor {
      poorKey   :: EncKeyPair
    , poorAddrs :: [(EncKeyPair, Address)]
    }
  deriving (Show)

data Stakeholder = Stakeholder {
      stkKey :: RegularKeyPair
    , stkDel :: DelegatedTo Rich
    }
  deriving (Show)

-- | AVVM acount
data Avvm = Avvm {
      avvmKey  :: RedeemKeyPair
    , avvmSeed :: ByteString
    , avvmAddr :: Address
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
    actorsAvvm  :: Map RedeemPublicKey Avvm

    actorsRich  = Map.fromList $ map mkRich  $ gsRichSecrets       ccSecrets
    actorsPoor  = Map.fromList $ map mkPoor  $ gsPoorSecrets       ccSecrets
    actorsStake = Map.fromList $ map mkStake $ gsDlgIssuersSecrets ccSecrets
    actorsAvvm  = Map.fromList $ map mkAvvm  $ gsFakeAvvmSeeds     ccSecrets

    -- Intentially not using record wildcards here so that we fail to compile
    -- when the structure of the record changes.

    mkRich :: RichSecrets -> (PublicKey, Rich)
    mkRich (RichSecrets richSec _vss) =
        (regKpPub richKey, Rich {..})
      where
        richKey :: RegularKeyPair
        richKey = regularKeyPair richSec

        richAddr :: Address
        richAddr = makePubKeyAddressBoot (toPublic richSec)

    mkPoor :: PoorSecret -> (PublicKey, Poor)
    mkPoor (PoorSecret _) = error err
      where
        err :: Text
        err = sformat (
              "Unexpected unecrypted secret key "
            % "(this is only used in non-HD mode)"
            )
    mkPoor (PoorEncryptedSecret poorSec) = (encKpPub poorKey, Poor {..})
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
    mkStake stkSec = (regKpHash stkKey, Stakeholder{..})
      where
        stkKey :: RegularKeyPair
        stkKey = regularKeyPair stkSec

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
                   (regKpHash stkKey)
                   (unGenesisDelegation $ gdHeavyDelegation ccData)

    mkAvvm :: ByteString -> (RedeemPublicKey, Avvm)
    mkAvvm avvmSeed = (redKpPub, Avvm{..})
      where
        avvmKey :: RedeemKeyPair
        avvmKey = RedeemKeyPair{..}

        avvmAddr :: Address
        avvmAddr = makeRedeemAddress redKpPub

        Just (redKpPub, redKpSec) = redeemDeterministicKeyGen avvmSeed

{-------------------------------------------------------------------------------
  In many cases the difference between rich and poor actors is not important
-------------------------------------------------------------------------------}

-- | Index the actors by number
data ActorIx
  = IxRich Int
  | IxPoor Int
  | IxAvvm Int
  -- ^ AVVM refers to the special accounts set up at the start of the Cardano
  -- blockchain that could then be redeemed from, once, for an initial balance.
  -- They can never receive a deposit.
  deriving (Show, Eq, Ord)

-- | Address index of a regular actor
--
-- We don't track the difference between the various actors at the type
-- level to make things a bit more uniform.
type AddrIx = Int

-- | Address is given by an actor index and an address index
data Addr = Addr {
      addrActorIx :: ActorIx
    , addrIx      :: AddrIx
    }
  deriving (Show, Eq, Ord)

-- | The maximum size in bytes of the serialized Cardano form of these addresses
--
-- This is needed for fee estimation.
maxAddrSize :: Int
maxAddrSize = error "TODO: maxAddrSize: not defined!"

-- | Returns true if this is the address of an AVVM account
isAvvmAddr :: Addr -> Bool
isAvvmAddr addr =
    case addrActorIx addr of
        IxAvvm _ -> True
        _        -> False

-- | Returns true if this is the address of a poor actor
isPoorAddr :: Addr -> Bool
isPoorAddr addr =
    case addrActorIx addr of
        IxPoor _ -> True
        _        -> False

-- | Information about the translation of a DSL address
data AddrInfo = AddrInfo {
      -- | The master key for the actor owning this address (for HD addresses)
      addrInfoMasterKey :: Maybe EncKeyPair

      -- | The key for this particular address
    , addrInfoAddrKey   :: SomeKeyPair

      -- | The Cardano address
    , addrInfoCardano   :: Address
    }

-- | Mapping between our addresses and Cardano addresses
data AddrMap = AddrMap {
      -- | Map from the DSL address to 'AddrInfo'
      addrMap    :: Map Addr AddrInfo

      -- | Reverse map from Cardano addresses to DSL addresses
    , addrRevMap :: Map Address Addr
    }

-- | Compute initial address mapping
initAddrMap :: Actors -> AddrMap
initAddrMap Actors{..} = AddrMap{
      addrMap    = Map.fromList mkMap
    , addrRevMap = Map.fromList $ map (swap . second addrInfoCardano) mkMap
    }
  where
    mkMap :: [(Addr, AddrInfo)]
    mkMap = concat [
                   zipWith mkRich [0..] (Map.elems actorsRich)
        , concat $ zipWith mkPoor [0..] (Map.elems actorsPoor)
        ,          zipWith mkAvvm [0..] (Map.elems actorsAvvm)
        ]

    mkRich :: Int -> Rich -> (Addr, AddrInfo)
    mkRich actorIx Rich{..} = (
          Addr (IxRich actorIx) 0
        , AddrInfo {
              addrInfoMasterKey = Nothing
            , addrInfoAddrKey   = KeyPairRegular richKey
            , addrInfoCardano   = richAddr
            }
        )

    mkPoor :: Int -> Poor -> [(Addr, AddrInfo)]
    mkPoor actorIx Poor{..} = zipWith poorRawAddr [0..] poorAddrs
      where
        poorRawAddr :: Int
                    -> (EncKeyPair, Address)
                    -> (Addr, AddrInfo)
        poorRawAddr addrIx (ekp, addr) = (
              Addr (IxPoor actorIx) addrIx
            , AddrInfo {
                  addrInfoMasterKey = Just poorKey
                , addrInfoAddrKey   = KeyPairEncrypted ekp
                , addrInfoCardano   = addr
                }
            )

    mkAvvm :: Int -> Avvm -> (Addr, AddrInfo)
    mkAvvm actorIx Avvm{..} = (
          Addr (IxAvvm actorIx) 0
        , AddrInfo {
              addrInfoMasterKey = Nothing
            , addrInfoAddrKey   = KeyPairRedeem avvmKey
            , addrInfoCardano   = avvmAddr
            }
        )

{-------------------------------------------------------------------------------
  Translation context

  The environment we need to be able to do the translation between the DSL
  and Cardano types. Accumulation of the environments above.
-------------------------------------------------------------------------------}

data TransCtxt = TransCtxt {
      tcCardano :: CardanoContext
    , tcActors  :: Actors
    , tcAddrMap :: AddrMap
    }

initContext :: CardanoContext -> TransCtxt
initContext tcCardano = TransCtxt{..}
  where
    tcActors  = initActors  tcCardano
    tcAddrMap = initAddrMap tcActors

{-------------------------------------------------------------------------------
  Derived information
-------------------------------------------------------------------------------}

resolveAddr :: Addr -> TransCtxt -> AddrInfo
resolveAddr addr TransCtxt{..} =
    fromMaybe
      (error $ sformat ("resolveAddr: " % build % " not found") addr)
      (Map.lookup addr addrMap)
  where
    AddrMap{..} = tcAddrMap

resolveAddress :: Address -> TransCtxt -> Addr
resolveAddress addr TransCtxt{..} =
    fromMaybe
      (error $ sformat ("resolveAddress: " % build % " not found") addr)
      (Map.lookup addr addrRevMap)
  where
    AddrMap{..} = tcAddrMap

leaderForSlot :: SlotLeaders -> SlotId -> TransCtxt -> Stakeholder
leaderForSlot leaders slotId TransCtxt{..} = actorsStake Map.! leader
  where
    Actors{..}         = tcActors
    CardanoContext{..} = tcCardano

    leader :: StakeholderId
    leader = leaders NE.!! slotIx

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

    bsiLeader = regKpPub stkKey
    bsiKey    = regKpSec richKey
    bsiPSK    = delPSK

blockSignInfoForSlot :: SlotLeaders -> SlotId -> TransCtxt -> BlockSignInfo
blockSignInfoForSlot leaders slotId =
      blockSignInfo
    . leaderForSlot leaders slotId

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

instance Buildable Avvm where
  build Avvm{..} = bprint
      ( "Avvm"
      % "{ key:  " % build
      % ", seed: " % base16F
      % ", addr: " % build
      % "}"
      )
      avvmKey
      avvmSeed
      avvmAddr

instance Buildable Actors where
  build Actors{..} = bprint
      ( "Actors"
      % "{ rich:  " % listJson
      % ", poor:  " % listJson
      % ", stake: " % listJson
      % ", avvm:  " % listJson
      % "}"
      )
      (Map.elems actorsRich)
      (Map.elems actorsPoor)
      (Map.elems actorsStake)
      (Map.elems actorsAvvm)

instance Buildable ActorIx where
  build (IxRich ix) = bprint ("IxRich " % build) ix
  build (IxPoor ix) = bprint ("IxPoor " % build) ix
  build (IxAvvm ix) = bprint ("IxAvvm " % build) ix

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
      % "{ initLeaders:  " % listJson
      % ", stakes:       " % listJson
      % ", balances:     " % listJson
      % ", utxo:         " % mapJson
      % ", data:         " % build
      % "}"
      )
      ccInitLeaders
      (map (bprint pairF) (HM.toList ccStakes))
      (map (bprint pairF) ccBalances)
      ccUtxo
      ccData

instance Buildable AddrMap where
  build AddrMap{..} = bprint
      ( "AddrMap"
      % "{ revMap: " % mapJson
      % "}"
      )
      addrRevMap

instance Buildable TransCtxt where
  build TransCtxt{..} = bprint
      ( "TransCtxt"
      % "{ cardano: " % build
      % ", actors:  " % build
      % ", addrMap: " % build
      % "}"
      )
      tcCardano
      tcActors
      tcAddrMap

instance Buildable GenesisData where
  build GenesisData{..} = bprint
      ( "GenesisData"
      % "{ bootStakeholders: " % build
      % "}"
      )
      gdBootStakeholders
