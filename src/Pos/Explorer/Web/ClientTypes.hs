-- | Types for using in purescript-bridge

module Pos.Explorer.Web.ClientTypes
       ( CHash (..)
       , CAddress (..)
       , CTxId (..)
       , CBlockEntry (..)
       , CTxEntry (..)
       , CBlockSummary (..)
       , CAddressSummary (..)
       , CTxRelative (..)
       , CTxRelativeType (..)
       , CNetworkAddress (..)
       , CTxSummary (..)
       , TxInternal (..)
       , toCHash
       , fromCHash
       , fromCHash'
       , toCAddress
       , fromCAddress
       , toCTxId
       , fromCTxId
       , toBlockEntry
       , toTxEntry
       , toBlockSummary
       , toTxRelative
       , toPosixTime
       ) where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy   as BSL
import           Data.Time.Clock.POSIX  (POSIXTime)
import           Formatting             (sformat)
import           Servant.API            (FromHttpApiData (..))
import           Universum

import qualified Pos.Binary             as Bi
import           Pos.Crypto             (Hash, hash)
import           Pos.DB                 (MonadDB (..))
import qualified Pos.DB.GState          as GS
import           Pos.Merkle             (getMerkleRoot, mtRoot)
import           Pos.Slotting           (MonadSlots (..), getSlotStart)
import           Pos.Ssc.Class          (SscHelpersClass)
import           Pos.Types              (Address, Coin, MainBlock, Timestamp,
                                         addressF, blockTxs,
                                         decodeTextAddress, difficultyL, gbHeader,
                                         gbhConsensus, headerHash, mcdSlot, mkCoin,
                                         prevBlockL, sumCoins, unsafeAddCoin,
                                         unsafeIntegerToCoin)
import           Pos.Txp                (Tx (..), TxId, TxOut (..), _txOutputs)
import           Pos.Util.TimeWarp      (NetworkAddress)

-------------------------------------------------------------------------------------
-- Hash types
-------------------------------------------------------------------------------------

-- | Client hash
newtype CHash = CHash Text
    deriving (Show, Eq, Generic, Buildable, Hashable)

-- | Client address
newtype CAddress = CAddress Text
    deriving (Show, Eq, Generic, Buildable, Hashable)

-- | Client transaction id
newtype CTxId = CTxId CHash
    deriving (Show, Eq, Generic, Hashable)

-- | Transformation of core hash-types to client representations and vice versa
encodeHashHex :: Hash a -> Text
encodeHashHex = decodeUtf8 . B16.encode . Bi.encodeStrict

decodeHashHex :: Text -> Either Text (Hash a)
decodeHashHex = fmap Bi.decode . processRes . B16.decode . encodeUtf8
  where processRes (res, rest) =
            if BS.null rest
            then Right $ BSL.fromStrict res
            else Left $ "decodeHashHex: couldn't decode rest of hash: " <> decodeUtf8 rest

decodeHashHex' :: Text -> Hash a
decodeHashHex' = either (panic "decodeHashHex: invalid hash") identity . decodeHashHex

toCHash :: Hash a -> CHash
toCHash = CHash . encodeHashHex

fromCHash :: CHash -> Either Text (Hash a)
fromCHash (CHash h) = decodeHashHex h

fromCHash' :: CHash -> Hash a
fromCHash' (CHash h) = decodeHashHex' h

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

-- | List of block entries is returned from "get latest N blocks" endpoint
data CBlockEntry = CBlockEntry
    { cbeBlkHash    :: !CHash
    , cbeHeight     :: !Word
    , cbeTimeIssued :: !(Maybe POSIXTime)
    , cbeTxNum      :: !Word
    , cbeTotalSent  :: !Coin
    , cbeSize       :: !Word64
    , cbeRelayedBy  :: !(Maybe Text)
    } deriving (Show, Generic)

toPosixTime :: Timestamp -> POSIXTime
toPosixTime = (/ 1e6) . fromIntegral

toBlockEntry
    :: (SscHelpersClass ssc, MonadSlots m)
    => MainBlock ssc
    -> m CBlockEntry
toBlockEntry blk = do
    blkSlotStart <- getSlotStart $
                    blk ^. gbHeader . gbhConsensus . mcdSlot
    let cbeBlkHash = toCHash $ headerHash blk
        cbeHeight = fromIntegral $ blk ^. difficultyL
        cbeTimeIssued = blkSlotStart >>= (Just . toPosixTime)
        txs = blk ^. blockTxs
        cbeTxNum = fromIntegral $ length txs
        addCoins c = unsafeAddCoin c . totalTxMoney
        cbeTotalSent = foldl' addCoins (mkCoin 0) txs
        -- TODO: is there a way to get it more efficiently?
        cbeSize = fromIntegral . BSL.length $ Bi.encode blk
        cbeRelayedBy = Nothing
    return CBlockEntry {..}

-- | List of tx entries is returned from "get latest N transactions" endpoint
data CTxEntry = CTxEntry
    { cteId         :: !CTxId
    , cteTimeIssued :: !(Maybe POSIXTime)
    , cteAmount     :: !Coin
    } deriving (Show, Generic)

totalTxMoney :: Tx -> Coin
totalTxMoney = unsafeIntegerToCoin . sumCoins .
               map txOutValue . _txOutputs

toTxEntry :: Maybe Timestamp -> Tx -> CTxEntry
toTxEntry ts tx = CTxEntry {..}
  where cteId = toCTxId $ hash tx
        cteTimeIssued = ts >>= (Just . toPosixTime)
        cteAmount = totalTxMoney tx

-- | Data displayed on block summary page
data CBlockSummary = CBlockSummary
    { cbsEntry      :: !CBlockEntry
    , cbsPrevHash   :: !CHash
    , cbsNextHash   :: !(Maybe CHash)
    , cbsMerkleRoot :: !CHash
    } deriving (Show, Generic)

toBlockSummary
    :: (SscHelpersClass ssc, MonadSlots m, MonadDB ssc m)
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
    , caTxList  :: ![CTxRelative]
    } deriving (Show, Generic)

data CTxRelative = CTxRelative
    { ctrId         :: !CTxId
    , ctrTimeIssued :: !(Maybe POSIXTime)
    , ctrType       :: !CTxRelativeType
    } deriving (Show, Generic)

data CTxRelativeType =
      CTxIncoming { ctiIncomingAddresses :: ![CAddress]
                  , ctiIncomingAmount    :: !Coin}
    -- TODO: Add these constructors when we can provide relevant data
    -- | CTxOutgoing { ctiOutgoingAddresses :: ![CAddress]
    --               , ctiOutgoingAmount    :: !Coin}
    -- | CTxBoth     { ctiIncomingAddresses :: ![CAddress]
    --               , ctiIncomingAmount    :: !Coin
    --               , ctiOutgoingAddresses :: ![CAddress]
    --               , ctiOutgoingAmount    :: !Coin}
    deriving (Show, Generic)

data CNetworkAddress = CNetworkAddress !Text
    deriving (Show, Generic)

data CTxSummary = CTxSummary
    { ctsId              :: !CTxId
    , ctsTxTimeIssued    :: !(Maybe POSIXTime)
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

--------------------------------------------------------------------------------
-- Helper types and conversions
--------------------------------------------------------------------------------

data TxInternal = TxInternal
    { tiTimestamp :: !(Maybe Timestamp)
    , tiTx        :: !Tx
    } deriving (Show)

toTxRelative :: Address -> TxInternal -> CTxRelative
toTxRelative addr txi = CTxRelative {..}
  where
    tx = tiTx txi
    ts = tiTimestamp txi
    ctrId = toCTxId $ hash tx
    ctrTimeIssued = ts >>= (Just . toPosixTime)
    amount = unsafeIntegerToCoin . sumCoins . map txOutValue .
             filter (\txOut -> txOutAddress txOut == addr) . _txOutputs $ tx
    ctrType = CTxIncoming [] amount