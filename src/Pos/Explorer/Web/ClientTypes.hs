-- | Types for using in purescript-bridge

module Pos.Explorer.Web.ClientTypes
       ( CHash (..)
       , CAddress (..)
       , CTxId (..)
       , CBlockEntry (..)
       , toBlockEntry
       , CTxEntry (..)
       , toTxEntry
       ) where

import qualified Data.ByteString.Lazy  as BSL
import           Data.Time.Clock.POSIX (POSIXTime)
import           Formatting            (build, sformat)
import           Universum

import           Pos.Binary            (encode)
import           Pos.Crypto            (Hash, hash)
import           Pos.Slotting          (MonadSlots (..), getSlotStart)
import           Pos.Ssc.Class         (SscHelpersClass)
import           Pos.Types             (Address, Coin, MainBlock (..), Timestamp, Tx (..),
                                        TxId, TxOut (..), addressF, blockTxs, difficultyL,
                                        gbHeader, gbhConsensus, mcdSlot, mkCoin, sumCoins,
                                        unsafeAddCoin, unsafeIntegerToCoin)

-- | Client hash
newtype CHash = CHash Text deriving (Show, Eq, Generic, Buildable, Hashable)

-- | Client address
newtype CAddress = CAddress Text deriving (Show, Eq, Generic, Hashable, Buildable)

-- | Client transaction id
newtype CTxId = CTxId CHash deriving (Show, Eq, Generic, Hashable)

-- | Transformation of core hash-types to client representations
toCHash :: Hash a -> CHash
toCHash = CHash . sformat build

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

toTxEntry :: POSIXTime -> Tx -> CTxEntry
toTxEntry cteTimeIssued tx = CTxEntry {..}
  where cteId = toCTxId $ hash tx
        cteAmount = totalTxMoney tx

