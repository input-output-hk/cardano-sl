-- | Types for using in purescript-bridge

module Pos.Explorer.Web.ClientTypes
       ( CHash (..)
       , CAddress (..)
       , CTxId (..)
       , CBlockEntry (..)
       , CTxEntry (..)
       , CBlockSummary (..)
       , toCHash
       , fromCHash
       , toCAddress
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

import           Pos.Binary             (encode)
import           Pos.Crypto             (Hash, decodeHash, encodeHash, hash)
import           Pos.DB                 (MonadDB (..))
import qualified Pos.DB.GState          as GS
import           Pos.Merkle             (getMerkleRoot, mtRoot)
import           Pos.Slotting           (MonadSlots (..), getSlotStart)
import           Pos.Ssc.Class          (SscHelpersClass)
import           Pos.Types              (Address, Coin, MainBlock (..), Timestamp,
                                         Tx (..), TxId, TxOut (..), addressF, blockTxs,
                                         difficultyL, gbHeader, gbhConsensus, headerHash,
                                         mcdSlot, mkCoin, prevBlockL, sumCoins,
                                         unsafeAddCoin, unsafeIntegerToCoin)

-- | Client hash
newtype CHash = CHash Text deriving (Show, Eq, Generic, Buildable, Hashable)

-- | Client address
newtype CAddress = CAddress Text deriving (Show, Eq, Generic, Hashable, Buildable)

-- | Client transaction id
newtype CTxId = CTxId CHash deriving (Show, Eq, Generic, Hashable)

-- | Transformation of core hash-types to client representations and vice versa
toCHash :: Hash a -> CHash
toCHash = CHash . encodeHash

fromCHash :: CHash -> Hash a
fromCHash (CHash h) = decodeHash h

toCAddress :: Address -> CAddress
toCAddress = CAddress . sformat addressF

toCTxId :: TxId -> CTxId
toCTxId = CTxId . toCHash

-- | List of block entries is returned from "get latest N blocks" endpoint
data CBlockEntry = CBlockEntry
    { cbeHeight     :: !Word
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
    let cbeHeight = fromIntegral $ blk ^. difficultyL
        cbeTimeIssued = toPosixTime blkSlotStart
        txs = blk ^. blockTxs
        cbeTxNum = fromIntegral $ length txs
        addCoins c = unsafeAddCoin c . totalTxMoney
        cbeTotalSent = foldl' addCoins (mkCoin 0) txs
        -- TODO: is there a way to get it more efficiently?
        cbeSize = fromIntegral . BSL.length $ encode blk
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
    , cbsBlkHash    :: !CHash
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
    let cbsBlkHash = toCHash $ headerHash blk
        cbsPrevHash = toCHash $ blk ^. prevBlockL
        cbsMerkleRoot = toCHash . getMerkleRoot . mtRoot $ blk ^. blockTxs
    return CBlockSummary {..}

-------------------------------------------------------------------------------------
-- FromHttpApiData instances
-------------------------------------------------------------------------------------

instance FromHttpApiData CHash where
    parseUrlPiece = pure . CHash
