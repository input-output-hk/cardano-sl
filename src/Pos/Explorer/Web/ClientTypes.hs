-- | Types for using in purescript-bridge

module Pos.Explorer.Web.ClientTypes
       ( CSearchId (..)
       , CHash (..)
       , CAddress (..)
       , CTxId (..)
       , CHashSearchResult (..)
       , CBlockEntry (..)
       , CTxEntry (..)
       , CBlockSummary (..)
       , CAddressSummary (..)
       , CTxBrief (..)
       , CNetworkAddress (..)
       , CTxSummary (..)
       , TxInternal (..)
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
       , toPosixTime
       , fromCSearchIdHash
       , fromCSearchIdAddress
       , fromCSearchIdTx
       , convertTxOutputs
       , tiToTxEntry
       ) where

import           Control.Arrow          ((&&&))
import           Control.Lens           (_Left)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.List.NonEmpty     as NE
import           Data.Time.Clock.POSIX  (POSIXTime)
import           Formatting             (sformat)
import           Serokell.Util.Base16   as SB16
import           Servant.API            (FromHttpApiData (..))
import           Universum

import qualified Pos.Binary             as Bi
import           Pos.Crypto             (Hash, hash)
import           Pos.DB                 (MonadDB (..))
import qualified Pos.DB.GState          as GS
import           Pos.Merkle             (getMerkleRoot, mtRoot)
import           Pos.Slotting           (MonadSlots (..), getSlotStart)
import           Pos.Ssc.Class          (SscHelpersClass)
import           Pos.Txp                (Tx (..), TxId, TxOut (..), TxOutAux (..),
                                         _txOutputs)
import           Pos.Types              (Address, Coin, MainBlock, SlotId (..), Timestamp,
                                         addressF, blockSlot, blockTxs, decodeTextAddress,
                                         gbHeader, gbhConsensus, getEpochIndex,
                                         getSlotIndex, headerHash, mcdSlot, mkCoin,
                                         prevBlockL, sumCoins, unsafeAddCoin,
                                         unsafeIntegerToCoin)
import           Pos.Types.Explorer     (TxExtra (..))



-------------------------------------------------------------------------------------
-- Hash types
-------------------------------------------------------------------------------------

-- | Client hash
newtype CSearchId = CSearchId Text
    deriving (Show, Eq, Generic, Buildable, Hashable)

-- | Client hash
newtype CHash = CHash Text
    deriving (Show, Eq, Generic, Buildable, Hashable)

-- | Client address
newtype CAddress = CAddress Text
    deriving (Show, Eq, Generic, Buildable, Hashable)

-- | Client transaction id
newtype CTxId = CTxId CHash
    deriving (Show, Eq, Generic, Buildable, Hashable)

-- | Transformation of core hash-types to client representations and vice versa
encodeHashHex :: Hash a -> Text
encodeHashHex = decodeUtf8 . B16.encode . Bi.encodeStrict

decodeHashHex :: Text -> Either Text (Hash a)
decodeHashHex hashText = do
    hashBinary <- SB16.decode hashText
    over _Left toText $ Bi.decodeFull $ BSL.fromStrict hashBinary

toCSearchId :: Hash a -> CSearchId
toCSearchId = CSearchId . encodeHashHex

-- fromCSearchId :: CSearchId -> Either Text (Hash a)
-- fromCSearchId (CSearchId hashId) = decodeHashHex hashId

--TODO: Iso?
fromCSearchIdHash :: CSearchId -> CHash
fromCSearchIdHash (CSearchId hashId) = CHash hashId

fromCSearchIdAddress :: CSearchId -> CAddress
fromCSearchIdAddress (CSearchId hashId) = CAddress hashId

fromCSearchIdTx :: CSearchId -> CTxId
fromCSearchIdTx sid = CTxId $ fromCSearchIdHash sid

toCHash :: Hash a -> CHash
toCHash = CHash . encodeHashHex

fromCHash :: CHash -> Either Text (Hash a)
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

-- | Client search result when the client searches by hash.
data CHashSearchResult
    = AddressFound CAddressSummary
    | BlockFound CBlockSummary
    | TransactionFound CTxSummary
    deriving (Show, Generic)

-- | List of block entries is returned from "get latest N blocks" endpoint
data CBlockEntry = CBlockEntry
    { cbeEpoch      :: !Word64
    , cbeSlot       :: !Word16
    , cbeBlkHash    :: !CHash
    , cbeTimeIssued :: !(Maybe POSIXTime)
    , cbeTxNum      :: !Word
    , cbeTotalSent  :: !Coin
    , cbeSize       :: !Word64
    , cbeRelayedBy  :: !(Maybe Text)
    } deriving (Show, Generic)

toPosixTime :: Timestamp -> POSIXTime
toPosixTime = (/ 1e6) . fromIntegral

toBlockEntry
    :: (SscHelpersClass ssc, MonadSlots m, MonadThrow m)
    => MainBlock ssc
    -> m CBlockEntry
toBlockEntry blk = do
    blkSlotStart <- getSlotStart (blk ^. gbHeader . gbhConsensus . mcdSlot)
    let headerSlot    = blk ^. blockSlot
        cbeEpoch      = getEpochIndex $ siEpoch headerSlot
        cbeSlot       = getSlotIndex  $ siSlot  headerSlot
        cbeBlkHash    = toCHash $ headerHash blk
        cbeTimeIssued = toPosixTime <$> blkSlotStart
        txs           = blk ^. blockTxs
        cbeTxNum      = fromIntegral $ length txs
        addCoins c    = unsafeAddCoin c . totalTxMoney
        cbeTotalSent  = foldl' addCoins (mkCoin 0) txs
        -- TODO: is there a way to get it more efficiently?
        cbeSize       = fromIntegral . BSL.length $ Bi.encode blk
        cbeRelayedBy  = Nothing
    return CBlockEntry {..}


-- | List of tx entries is returned from "get latest N transactions" endpoint
data CTxEntry = CTxEntry
    { cteId         :: !CTxId
    , cteTimeIssued :: !POSIXTime
    , cteAmount     :: !Coin
    } deriving (Show, Generic)

totalTxMoney :: Tx -> Coin
totalTxMoney = unsafeIntegerToCoin . sumCoins .
               map txOutValue . _txOutputs

toTxEntry :: Timestamp -> Tx -> CTxEntry
toTxEntry ts tx = CTxEntry {..}
  where cteId = toCTxId $ hash tx
        cteTimeIssued = toPosixTime ts
        cteAmount = totalTxMoney tx

-- | Data displayed on block summary page
data CBlockSummary = CBlockSummary
    { cbsEntry      :: !CBlockEntry
    , cbsPrevHash   :: !CHash
    , cbsNextHash   :: !(Maybe CHash)
    , cbsMerkleRoot :: !CHash
    } deriving (Show, Generic)

toBlockSummary
    :: (SscHelpersClass ssc, MonadSlots m, MonadDB m)
    => MainBlock ssc
    -> m CBlockSummary
toBlockSummary blk = do
    cbsEntry <- toBlockEntry blk
    cbsNextHash <- fmap toCHash <$> GS.resolveForwardLink blk
    let cbsPrevHash = toCHash $ blk ^. prevBlockL
        cbsMerkleRoot = toCHash . getMerkleRoot . mtRoot $ blk ^. blockTxs
    return CBlockSummary {..}

data CAddressSummary = CAddressSummary
    { caAddress :: !CAddress
    , caTxNum   :: !Word
    , caBalance :: !Coin
    , caTxList  :: ![CTxBrief]
    } deriving (Show, Generic)

data CTxBrief = CTxBrief
    { ctbId         :: !CTxId
    , ctbTimeIssued :: !POSIXTime
    , ctbInputs     :: ![(CAddress, Coin)]
    , ctbOutputs    :: ![(CAddress, Coin)]
    } deriving (Show, Generic)

-- FIXME: newtype?
data CNetworkAddress = CNetworkAddress !Text
    deriving (Show, Generic)

data CTxSummary = CTxSummary
    { ctsId              :: !CTxId
    , ctsTxTimeIssued    :: !POSIXTime
    , ctsBlockTimeIssued :: !(Maybe POSIXTime)
    , ctsBlockHeight     :: !(Maybe Word)
    , ctsRelayedBy       :: !(Maybe CNetworkAddress)
    , ctsTotalInput      :: !Coin
    , ctsTotalOutput     :: !Coin
    , ctsFees            :: !Coin
    , ctsInputs          :: ![(CAddress, Coin)]
    , ctsOutputs         :: ![(CAddress, Coin)]
    } deriving (Show, Generic)

--------------------------------------------------------------------------------
-- FromHttpApiData instances
--------------------------------------------------------------------------------

instance FromHttpApiData CHash where
    parseUrlPiece = fmap toCHash . decodeHashHex

instance FromHttpApiData CAddress where
    parseUrlPiece = fmap toCAddress . decodeTextAddress

instance FromHttpApiData CTxId where
    parseUrlPiece = fmap toCTxId . decodeHashHex

instance FromHttpApiData CSearchId where
    parseUrlPiece = fmap toCSearchId . decodeHashHex

--------------------------------------------------------------------------------
-- Helper types and conversions
--------------------------------------------------------------------------------

data TxInternal = TxInternal
    { tiTimestamp :: !Timestamp
    , tiTx        :: !Tx
    } deriving (Show, Eq, Ord)

tiToTxEntry :: TxInternal -> CTxEntry
tiToTxEntry TxInternal{..} = toTxEntry tiTimestamp tiTx

convertTxOutputs :: [TxOut] -> [(CAddress, Coin)]
convertTxOutputs = map (toCAddress . txOutAddress &&& txOutValue)

toTxBrief :: TxInternal -> TxExtra -> CTxBrief
toTxBrief txi txe = CTxBrief {..}
  where
    tx = tiTx txi
    ts = tiTimestamp txi
    ctbId = toCTxId $ hash tx
    ctbTimeIssued = toPosixTime ts
    ctbInputs = convertTxOutputs $ map toaOut $ NE.toList $ teInputOutputs txe
    ctbOutputs = convertTxOutputs . NE.toList $ _txOutputs tx
