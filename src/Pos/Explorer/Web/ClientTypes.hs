-- | Types for using in purescript-bridge

module Pos.Explorer.Web.ClientTypes
       ( CHash (..)
       , CAddress (..)
       , CTxId (..)
       , CBlockEntry (..)
       , CTxEntry (..)
       , CBlockSummary (..)
       , CAddressSummary (..)
       , toCHash
       , fromCHash
       , fromCHash'
       , toCAddress
       , fromCAddress
       , toCTxId
       , toBlockEntry
       , toTxEntry
       , toBlockSummary
       ) where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy   as BSL
import           Data.Time.Clock.POSIX  (POSIXTime)
import           Formatting             (build, sformat)
import           Servant.API            (FromHttpApiData (..))
import           Universum

import qualified Pos.Binary             as Bi
import           Pos.Crypto             (Hash, hash)
import           Pos.DB                 (MonadDB (..))
import qualified Pos.DB.GState          as GS
import           Pos.Merkle             (getMerkleRoot, mtRoot)
import           Pos.Slotting           (MonadSlots (..), getSlotStart)
import           Pos.Ssc.Class          (SscHelpersClass)
import           Pos.Types              (Address, Coin, MainBlock (..), Timestamp,
                                         Tx (..), TxId, TxOut (..), addressF, blockTxs,
                                         decodeTextAddress, difficultyL, gbHeader,
                                         gbhConsensus, headerHash, mcdSlot, mkCoin,
                                         prevBlockL, sumCoins, unsafeAddCoin,
                                         unsafeIntegerToCoin)

-------------------------------------------------------------------------------------
-- Hash types
-------------------------------------------------------------------------------------

-- | Client hash
newtype CHash = CHash Text deriving (Show, Eq, Generic, Buildable, Hashable)

-- | Client address
newtype CAddress = CAddress Text deriving (Show, Eq, Generic, Hashable, Buildable)

-- | Client transaction id
newtype CTxId = CTxId CHash deriving (Show, Eq, Generic, Hashable)

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

-------------------------------------------------------------------------------------
-- Composite types
-------------------------------------------------------------------------------------

-- | List of block entries is returned from "get latest N blocks" endpoint
data CBlockEntry = CBlockEntry
    { cbeBlkHash    :: !CHash
    , cbeHeight     :: !Word
    , cbeTimeIssued :: !POSIXTime
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
        cbeTimeIssued = toPosixTime blkSlotStart
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
    , cteTimeIssued :: !POSIXTime
    , cteAmount     :: !Coin
    } deriving (Show, Generic)

totalTxMoney :: Tx -> Coin
totalTxMoney = unsafeIntegerToCoin . sumCoins .
               map txOutValue . txOutputs

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
    -- , caTxList  :: ![CTxDetailed]
    } deriving (Show, Generic)

data CTxDetailed = CTxDetailed
    { ctdId         :: !CTxId
    , ctdTimeIssued :: !POSIXTime
    , ctdType       :: !CTxType
    } deriving (Show, Generic)

data CTxType =
      CTxIncoming ![CAddress] !Coin
    | CTxOutgoing ![CAddress] !Coin
    | CTxBoth     ![CAddress] !Coin ![CAddress] !Coin
    deriving (Show, Generic)

-------------------------------------------------------------------------------------
-- FromHttpApiData instances
-------------------------------------------------------------------------------------

instance FromHttpApiData CHash where
    parseUrlPiece = fmap toCHash . decodeHashHex

instance FromHttpApiData CAddress where
    parseUrlPiece = fmap toCAddress . decodeTextAddress
