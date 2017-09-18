{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-# LANGUAGE RankNTypes          #-}

-- | Types for using in purescript-bridge
module Pos.Explorer.Web.ClientTypes
       ( CHash (..)
       , CAddress (..)
       , CTxId (..)
       , CBlockEntry (..)
       , CTxEntry (..)
       , CBlockSummary (..)
       , CAddressType (..)
       , CAddressSummary (..)
       , CTxBrief (..)
       , CNetworkAddress (..)
       , CTxSummary (..)
       , CGenesisSummary (..)
       , CGenesisAddressInfo (..)
       , TxInternal (..)
       , CCoin
       , EpochIndex (..)
       , LocalSlotIndex (..)
       , StakeholderId
       , Byte
       , mkCCoin
       , mkCCoinMB
       , toCHash
       , fromCHash
       , toCAddress
       , fromCAddress
       , toCTxId
       , fromCTxId
       , toBlockEntry
       , toTxEntry
       , toBlockSummary
       , toTxBrief
       , timestampToPosix
       , convertTxOutputs
       , convertTxOutputsMB
       , tiToTxEntry
       , encodeHashHex
       , decodeHashHex
       ) where

import           Universum

import           Control.Arrow              ((&&&))
import           Control.Lens               (ix, _Left)
import qualified Data.ByteArray             as BA
import qualified Data.List.NonEmpty         as NE
import           Data.Time.Clock.POSIX      (POSIXTime)
import           Formatting                 (sformat)
import           Pos.Binary                 (Bi, biSize)
import           Pos.Block.Core             (MainBlock, mainBlockSlot, mainBlockTxPayload,
                                             mcdSlot)
import           Pos.Block.Types            (Undo (..))
import           Pos.Core                   (HasConfiguration, timestampToPosix)
import           Pos.Crypto                 (Hash, hash)
import           Pos.DB.Block               (MonadBlockDB)
import           Pos.DB.Class               (MonadDBRead)
import           Pos.DB.Rocks               (MonadRealDB)
import           Pos.Explorer               (TxExtra (..))
import qualified Pos.GState                 as GS
import           Pos.Lrc                    (getLeaders)
import           Pos.Merkle                 (getMerkleRoot, mtRoot)
import           Pos.Slotting               (MonadSlots (..), getSlotStart)
import           Pos.Ssc.GodTossing         (SscGodTossing)
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.Txp                    (Tx (..), TxId, TxOut (..), TxOutAux (..),
                                             TxUndo, txpTxs, _txOutputs)
import           Pos.Types                  (Address, AddressHash, Coin, EpochIndex,
                                             LocalSlotIndex, SlotId (..), StakeholderId,
                                             Timestamp, addressF, coinToInteger,
                                             decodeTextAddress, gbHeader, gbhConsensus,
                                             getEpochIndex, getSlotIndex, headerHash,
                                             mkCoin, prevBlockL, sumCoins, unsafeAddCoin,
                                             unsafeGetCoin, unsafeIntegerToCoin,
                                             unsafeSubCoin)
import           Prelude                    ()
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util.Base16       as SB16
import           Servant.API                (FromHttpApiData (..))


-------------------------------------------------------------------------------------
-- Hash types
-------------------------------------------------------------------------------------

-- See this page for more explanation - https://cardanodocs.com/cardano/addresses/
-- We have the general type @AbstractHash@ for all hashes we use. It's being parametrized
-- by two types - AbstractHash algo a - the hashing algorithm and the phantom type for
-- extra safety (can be a @Tx@, an @Address@ and so on, ...).
--
-- The following types explain the situation better:
--
-- type AddressHash = AbstractHash Blake2b_224
-- type Hash        = AbstractHash Blake2b_256
-- type TxId        = Hash Tx
--
-- From there on we have the client types that we use to represent the actual hashes.
-- The client types are really the hash bytes converted to Base16 address.

-- | Client hash
newtype CHash = CHash Text
  deriving (Show, Eq, Generic, Buildable, Hashable)

-- | Client address. The address may be from either Cardano or RSCoin.
newtype CAddress = CAddress Text
    deriving (Show, Eq, Generic, Buildable, Hashable)

-- | Client transaction id
newtype CTxId = CTxId CHash
    deriving (Show, Eq, Generic, Buildable, Hashable)

-------------------------------------------------------------------------------------
-- Client-server, server-client transformation functions
-------------------------------------------------------------------------------------

-- | Transformation of core hash-types to client representations and vice versa
encodeHashHex :: forall a. (Bi a) => Hash a -> Text
encodeHashHex = SB16.encode . BA.convert

-- | We need this for stakeholders
encodeAHashHex :: forall a. (Bi a) => AddressHash a -> Text
encodeAHashHex = SB16.encode . BA.convert

-- | A required instance for decoding.
instance ToString ByteString where
  toString = toString . SB16.encode

-- | Decoding the text to the original form.
decodeHashHex :: forall a. (Bi (Hash a)) => Text -> Either Text (Hash a)
decodeHashHex hashText = do
    hashBinary <- SB16.decode hashText
    over _Left toText $ readEither hashBinary

-------------------------------------------------------------------------------------
-- Client hashes functions
-------------------------------------------------------------------------------------

toCHash :: forall a. (Bi a) => Hash a -> CHash
toCHash = CHash . encodeHashHex

fromCHash :: forall a. (Bi (Hash a)) => CHash -> Either Text (Hash a)
fromCHash (CHash h) = decodeHashHex h

toCAddress :: Address -> CAddress
toCAddress = CAddress . sformat addressF

fromCAddress :: CAddress -> Either Text Address
fromCAddress (CAddress addr) = decodeTextAddress addr

toCTxId :: TxId -> CTxId
toCTxId = CTxId . toCHash

fromCTxId :: CTxId -> Either Text TxId
fromCTxId (CTxId (CHash txId)) = decodeHashHex txId

-------------------------------------------------------------------------------------
-- Composite types
-------------------------------------------------------------------------------------

newtype CCoin = CCoin
    { getCoin :: Text
    } deriving (Show, Generic)

mkCCoin :: Coin -> CCoin
mkCCoin = CCoin . show . unsafeGetCoin

mkCCoinMB :: Maybe Coin -> CCoin
mkCCoinMB = maybe (CCoin "N/A") mkCCoin

-- | List of block entries is returned from "get latest N blocks" endpoint
data CBlockEntry = CBlockEntry
    { cbeEpoch      :: !Word64
    , cbeSlot       :: !Word16
    , cbeBlkHash    :: !CHash
    , cbeTimeIssued :: !(Maybe POSIXTime)
    , cbeTxNum      :: !Word
    , cbeTotalSent  :: !CCoin
    , cbeSize       :: !Word64
    , cbeBlockLead  :: !(Maybe Text) -- todo (ks): Maybe CAddress?
    , cbeFees       :: !CCoin
    } deriving (Show, Generic)

toBlockEntry
    :: forall ctx m .
    ( MonadBlockDB SscGodTossing m
    , MonadDBRead m
    , MonadRealDB ctx m
    , MonadSlots ctx m
    , MonadThrow m
    , HasConfiguration
    , HasGtConfiguration
    )
    => (MainBlock SscGodTossing, Undo)
    -> m CBlockEntry
toBlockEntry (blk, Undo{..}) = do

    blkSlotStart      <- getSlotStart (blk ^. gbHeader . gbhConsensus . mcdSlot)

    -- Get the header slot, from which we can fetch epoch and slot index.
    let blkHeaderSlot = blk ^. mainBlockSlot
        epochIndex    = siEpoch blkHeaderSlot
        slotIndex     = siSlot  blkHeaderSlot

    -- Find the epoch and slot leader
    epochSlotLeader   <- getLeaderFromEpochSlot epochIndex slotIndex

    -- Fill required fields for @CBlockEntry@
    let cbeEpoch      = getEpochIndex epochIndex
        cbeSlot       = getSlotIndex  slotIndex
        cbeBlkHash    = toCHash $ headerHash blk
        cbeTimeIssued = timestampToPosix <$> blkSlotStart
        txs           = toList $ blk ^. mainBlockTxPayload . txpTxs
        cbeTxNum      = fromIntegral $ length txs
        addOutCoins c = unsafeAddCoin c . totalTxOutMoney
        totalRecvCoin = unsafeIntegerToCoin . sumCoins <$> traverse totalTxInMoney undoTx
        totalSentCoin = foldl' addOutCoins (mkCoin 0) txs
        cbeTotalSent  = mkCCoin $ totalSentCoin
        cbeSize       = fromIntegral $ biSize blk
        cbeFees       = mkCCoinMB $ (`unsafeSubCoin` totalSentCoin) <$> totalRecvCoin

        -- A simple reconstruction of the AbstractHash, could be better?
        cbeBlockLead  = encodeAHashHex <$> epochSlotLeader


    return CBlockEntry {..}

-- | Get leader from epoch and slot in order to display them on the frontend.
-- Returning @Maybe@ is the simplest implementation for now, since it's hard
-- to forsee what is and what will the state of leaders be at any given moment.
getLeaderFromEpochSlot
    :: (MonadBlockDB SscGodTossing m, MonadDBRead m, MonadRealDB ctx m)
    => EpochIndex
    -> LocalSlotIndex
    -> m (Maybe StakeholderId)
getLeaderFromEpochSlot epochIndex slotIndex = do
    -- Get leaders from the database
    leadersMaybe <- getLeaders epochIndex
    -- If we have leaders for the given epoch, find the leader that is leading
    -- the slot we are interested in. If we find it, return it, otherwise
    -- return @Nothing@.
    pure $ leadersMaybe >>= \leaders -> leaders ^? ix intSlotIndex
  where
    intSlotIndex = fromIntegral $ getSlotIndex slotIndex

-- | List of tx entries is returned from "get latest N transactions" endpoint
data CTxEntry = CTxEntry
    { cteId         :: !CTxId
    , cteTimeIssued :: !(Maybe POSIXTime)
    , cteAmount     :: !CCoin
    } deriving (Show, Generic)

totalTxOutMoney :: Tx -> Coin
totalTxOutMoney =
    unsafeIntegerToCoin . sumCoins . map txOutValue . _txOutputs

totalTxInMoney :: TxUndo -> Maybe Coin
totalTxInMoney =
    fmap (unsafeIntegerToCoin . sumCoins . NE.map (txOutValue . toaOut)) . sequence

toTxEntry :: Maybe Timestamp -> Tx -> CTxEntry
toTxEntry ts tx = CTxEntry {..}
  where
    cteId         = toCTxId $ hash tx
    cteTimeIssued = timestampToPosix <$> ts
    cteAmount     = mkCCoin $ totalTxOutMoney tx

-- | Data displayed on block summary page
data CBlockSummary = CBlockSummary
    { cbsEntry      :: !CBlockEntry
    , cbsPrevHash   :: !CHash
    , cbsNextHash   :: !(Maybe CHash)
    , cbsMerkleRoot :: !CHash
    } deriving (Show, Generic)

toBlockSummary
    :: forall ctx m.
    ( MonadBlockDB SscGodTossing m
    , MonadDBRead m
    , MonadRealDB ctx m
    , MonadSlots ctx m
    , MonadThrow m
    , HasConfiguration
    , HasGtConfiguration
    )
    => (MainBlock SscGodTossing, Undo)
    -> m CBlockSummary
toBlockSummary blund@(blk, _) = do
    cbsEntry <- toBlockEntry blund
    cbsNextHash <- fmap toCHash <$> GS.resolveForwardLink blk

    let blockTxs      = blk ^. mainBlockTxPayload . txpTxs

    let cbsPrevHash   = toCHash $ blk ^. prevBlockL
    let cbsMerkleRoot = toCHash . getMerkleRoot . mtRoot $ blockTxs

    return CBlockSummary {..}

data CAddressType =
      CPubKeyAddress
    | CScriptAddress
    | CRedeemAddress
    | CUnknownAddress
    deriving (Show, Generic)

data CAddressSummary = CAddressSummary
    { caAddress :: !CAddress
    , caType    :: !CAddressType
    , caTxNum   :: !Word
    , caBalance :: !CCoin
    , caTxList  :: ![CTxBrief]
    } deriving (Show, Generic)

data CTxBrief = CTxBrief
    { ctbId         :: !CTxId
    , ctbTimeIssued :: !(Maybe POSIXTime)
    , ctbInputs     :: ![Maybe (CAddress, CCoin)]
    , ctbOutputs    :: ![(CAddress, CCoin)]
    , ctbInputSum   :: !CCoin
    , ctbOutputSum  :: !CCoin
    } deriving (Show, Generic)

newtype CNetworkAddress = CNetworkAddress Text
    deriving (Show, Generic)

data CTxSummary = CTxSummary
    { ctsId              :: !CTxId
    , ctsTxTimeIssued    :: !(Maybe POSIXTime)
    , ctsBlockTimeIssued :: !(Maybe POSIXTime)
    , ctsBlockHeight     :: !(Maybe Word)
    , ctsBlockEpoch      :: !(Maybe Word64)
    , ctsBlockSlot       :: !(Maybe Word16)
    , ctsBlockHash       :: !(Maybe CHash)
    , ctsRelayedBy       :: !(Maybe CNetworkAddress)
    , ctsTotalInput      :: !CCoin
    , ctsTotalOutput     :: !CCoin
    , ctsFees            :: !CCoin
    , ctsInputs          :: ![Maybe (CAddress, CCoin)]
    , ctsOutputs         :: ![(CAddress, CCoin)]
    } deriving (Show, Generic)

data CGenesisSummary = CGenesisSummary
    { cgsNumTotal    :: !Int
    , cgsNumRedeemed :: !Int
    } deriving (Show, Generic)

data CGenesisAddressInfo = CGenesisAddressInfo
    { cgaiCardanoAddress :: !CAddress
    -- Commenting out RSCoin address since currently genesisUtxo stores
    -- only Cardano addresses, which are essentially hashes of RSCoin addresses
    -- and therefore cannot be converted to them. Hence we should enable RSCoin
    -- addresses here only after we start storing them in genesisUtxo.
    -- , cgaiRSCoinAddress  :: !CAddress
    , cgaiGenesisAmount  :: !CCoin
    , cgaiIsRedeemed     :: !Bool
    } deriving (Show, Generic)

--------------------------------------------------------------------------------
-- FromHttpApiData instances
--------------------------------------------------------------------------------

instance FromHttpApiData CHash where
    -- Force the free type @a@ to a type `()` so we can get a witness
    -- for the `Bi` and `Typeable` instances.
    parseUrlPiece url = toCHash @() <$> decodeHashHex url

instance FromHttpApiData CAddress where
    parseUrlPiece = pure . CAddress

instance FromHttpApiData CTxId where
    parseUrlPiece = pure . CTxId . CHash

-- TODO: When we have a generic enough `readEither`
-- instance FromHttpApiData LocalSlotIndex where
--     parseUrlPiece = readEither

--------------------------------------------------------------------------------
-- Helper types and conversions
--------------------------------------------------------------------------------

data TxInternal = TxInternal
    { tiExtra :: !TxExtra
    , tiTx    :: !Tx
    } deriving (Show, Eq)

instance Ord TxInternal where
    compare = comparing tiTx

tiTimestamp :: TxInternal -> Maybe Timestamp
tiTimestamp = teReceivedTime . tiExtra

tiToTxEntry :: TxInternal -> CTxEntry
tiToTxEntry txi@TxInternal{..} = toTxEntry (tiTimestamp txi) tiTx

convertTxOutputsMB :: [Maybe TxOut] -> [Maybe (CAddress, Coin)]
convertTxOutputsMB = map (fmap $ toCAddress . txOutAddress &&& txOutValue)

convertTxOutputs :: [TxOut] -> [(CAddress, Coin)]
convertTxOutputs = map (toCAddress . txOutAddress &&& txOutValue)

toTxBrief :: TxInternal -> CTxBrief
toTxBrief txi = CTxBrief {..}
  where
    tx            = tiTx txi
    ts            = tiTimestamp txi
    ctbId         = toCTxId $ hash tx
    ctbTimeIssued = timestampToPosix <$> ts
    ctbInputs     = map (fmap (second mkCCoin)) txInputsMB
    ctbOutputs    = map (second mkCCoin) txOutputs
    ctbInputSum   = sumCoinOfInputsOutputs txInputsMB
    ctbOutputSum  = sumCoinOfInputsOutputs $ map Just txOutputs

    txInputsMB    = convertTxOutputsMB $ map (fmap toaOut) $ NE.toList $
                    teInputOutputs (tiExtra txi)
    txOutputs     = convertTxOutputs . NE.toList $ _txOutputs tx

-- | Sums the coins of inputs and outputs
sumCoinOfInputsOutputs :: [Maybe (CAddress, Coin)] -> CCoin
sumCoinOfInputsOutputs addressListMB
    | Just addressList <- sequence addressListMB = do
        -- | Get total number of coins from an address
        let addressCoins :: (CAddress, Coin) -> Integer
            addressCoins (_, coin) = coinToInteger coin

        -- | Arbitrary precision, so we don't overflow
        let addressCoinList :: [Integer]
            addressCoinList = addressCoins <$> addressList
        mkCCoin $ mkCoin $ fromIntegral $ sum addressCoinList
    | otherwise = mkCCoinMB Nothing
