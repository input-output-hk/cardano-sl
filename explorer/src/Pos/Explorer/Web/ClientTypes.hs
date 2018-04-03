{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Types that arise in the API: mostly simplified representations
-- of the core types which are easier to serialize.
-- Used in purescript-bridge.

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
       , CAddressesFilter (..)
       , TxInternal (..)
       , CCoin
       , CAda (..)
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

import qualified Prelude
import           Universum

import           Control.Arrow ((&&&))
import           Control.Lens (_Left)
import           Control.Monad.Error.Class (throwError)
import qualified Data.ByteArray as BA
import           Data.Default (Default (..))
import           Data.Fixed (Micro, showFixed)
import qualified Data.List.NonEmpty as NE
import           Data.Time.Clock.POSIX (POSIXTime)
import           Formatting (build, sformat, (%))
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util.Base16 as SB16
import           Servant.API (FromHttpApiData (..))
import           Test.QuickCheck (Arbitrary (..))

import           Pos.Binary (Bi, biSize)
import           Pos.Block.Types (Undo (..))
import           Pos.Core (Address, Coin, EpochIndex, LocalSlotIndex, SlotId (..), StakeholderId,
                           Timestamp, addressF, coinToInteger, decodeTextAddress, gbHeader,
                           gbhConsensus, getEpochIndex, getSlotIndex, headerHash, mkCoin,
                           prevBlockL, sumCoins, timestampToPosix, unsafeAddCoin, unsafeGetCoin,
                           unsafeIntegerToCoin, unsafeSubCoin)
import           Pos.Core.Block (MainBlock, mainBlockSlot, mainBlockTxPayload, mcdSlot)
import           Pos.Core.Txp (Tx (..), TxId, TxOut (..), TxOutAux (..), TxUndo, txpTxs, _txOutputs)
import           Pos.Crypto (AbstractHash, Hash, HashAlgorithm, hash)
import qualified Pos.GState as GS
import qualified Pos.Lrc as Lrc (getLeader)
import           Pos.Merkle (getMerkleRoot, mtRoot, mkMerkleTree)

import           Pos.Explorer.Core (TxExtra (..))
import           Pos.Explorer.ExplorerMode (ExplorerMode)
import           Pos.Explorer.ExtraContext (HasExplorerCSLInterface (..))
import           Pos.Explorer.TestUtil (secretKeyToAddress)
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
-- type AddressHash   = AbstractHash Blake2b_224
-- type Hash          = AbstractHash Blake2b_256
--
-- type TxId          = Hash Tx               = AbstractHash Blake2b_256 Tx
-- type StakeholderId = AddressHash PublicKey = AbstractHash Blake2b_224 PublicKey
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

-- | Transformation of core hash-types to client representation.
encodeHashHex
    :: forall algo a. (Bi a)
    => AbstractHash algo a
    -> Text
encodeHashHex = SB16.encode . BA.convert

-- | A required instance for decoding.
instance ToString ByteString where
  toString = toString . SB16.encode

-- | Decoding the text to the original form.
decodeHashHex
    :: forall algo a. (HashAlgorithm algo)
    => Text
    -> Either Text (AbstractHash algo a)
decodeHashHex hashText = do
  hashBinary <- SB16.decode hashText
  over _Left toText $ readEither hashBinary

-------------------------------------------------------------------------------------
-- Client hashes functions
-------------------------------------------------------------------------------------

toCHash :: forall a. (Bi a) => Hash a -> CHash
toCHash = CHash . encodeHashHex

fromCHash :: forall a. CHash -> Either Text (Hash a)
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
    } deriving (Show, Generic, Eq)

mkCCoin :: Coin -> CCoin
mkCCoin = CCoin . show . unsafeGetCoin

mkCCoinMB :: Maybe Coin -> CCoin
mkCCoinMB = maybe (CCoin "N/A") mkCCoin

newtype CAda = CAda
    { getAda :: Micro
    } deriving (Generic)

instance Show CAda where
    show (CAda ada) = showFixed True ada

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
    } deriving (Show, Generic, Eq)

toBlockEntry
    :: ExplorerMode ctx m
    => (MainBlock, Undo)
    -> m CBlockEntry
toBlockEntry (blk, Undo{..}) = do

    blkSlotStart      <- getSlotStartCSLI $ blk ^. gbHeader . gbhConsensus . mcdSlot

    -- Get the header slot, from which we can fetch epoch and slot index.
    let blkHeaderSlot = blk ^. mainBlockSlot
        epochIndex    = siEpoch blkHeaderSlot
        slotIndex     = siSlot  blkHeaderSlot

    -- Find the epoch and slot leader
    epochSlotLeader   <- Lrc.getLeader $ SlotId epochIndex slotIndex

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
        cbeBlockLead  = encodeHashHex <$> epochSlotLeader


    return CBlockEntry {..}


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
    :: ExplorerMode ctx m
    => (MainBlock, Undo)
    -> m CBlockSummary
toBlockSummary blund@(blk, _) = do
    cbsEntry    <- toBlockEntry blund
    cbsNextHash <- fmap toCHash <$> GS.resolveForwardLink blk

    let blockTxs      = blk ^. mainBlockTxPayload . txpTxs

    let cbsPrevHash   = toCHash $ blk ^. prevBlockL
    let cbsMerkleRoot = toCHash . getMerkleRoot . mtRoot . mkMerkleTree $ blockTxs

    return CBlockSummary {..}

data CAddressType
    = CPubKeyAddress
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
    { cgsNumTotal               :: !Int
    , cgsNumRedeemed            :: !Int
    , cgsNumNotRedeemed         :: !Int
    , cgsRedeemedAmountTotal    :: !CCoin
    , cgsNonRedeemedAmountTotal :: !CCoin
    } deriving (Show, Generic)

data CGenesisAddressInfo = CGenesisAddressInfo
    { cgaiCardanoAddress :: !CAddress
    , cgaiGenesisAmount  :: !CCoin
    , cgaiIsRedeemed     :: !Bool
    } deriving (Show, Generic)

data CAddressesFilter
    = RedeemedAddresses
    | NonRedeemedAddresses
    | AllAddresses
    deriving (Show, Generic)

instance Default CAddressesFilter where
    def = AllAddresses

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

instance FromHttpApiData CAddressesFilter where
    parseUrlPiece "all" = pure AllAddresses
    parseUrlPiece "redeemed" = pure RedeemedAddresses
    parseUrlPiece "notredeemed" = pure NonRedeemedAddresses
    parseUrlPiece other = throwError $
        sformat ("Unknown option '"%build%"'. "%
            "Valid options are 'all', 'redeemed' and 'notredeemed'.") other

-- TODO: When we have a generic enough `readEither`
-- instance FromHttpApiData LocalSlotIndex where
--     parseUrlPiece = readEither

--------------------------------------------------------------------------------
-- NFData instances
--------------------------------------------------------------------------------

instance NFData CBlockEntry
instance NFData CHash
instance NFData CCoin

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
        -- Get total number of coins from an address
        let addressCoins :: (CAddress, Coin) -> Integer
            addressCoins (_, coin) = coinToInteger coin

        -- Arbitrary precision, so we don't overflow
        let addressCoinList :: [Integer]
            addressCoinList = addressCoins <$> addressList
        mkCCoin $ mkCoin $ fromIntegral $ sum addressCoinList
    | otherwise = mkCCoinMB Nothing

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary CAddress where
    arbitrary = toCAddress . secretKeyToAddress <$> arbitrary
