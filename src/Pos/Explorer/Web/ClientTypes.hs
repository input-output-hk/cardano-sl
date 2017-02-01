-- | Types for using in purescript-bridge

module Pos.Explorer.Web.ClientTypes
       ( CHash (..)
       , CAddress (..)
       , CTxId (..)
       , CBlockEntry (..)
       , CTxEntry (..)
       ) where

import           Universum

import           Data.Time.Clock.POSIX (POSIXTime)
import           Pos.Types             (Coin)

-- | Client hash
newtype CHash = CHash Text deriving (Show, Eq, Generic, Buildable, Hashable)

-- | Client address
newtype CAddress = CAddress CHash deriving (Show, Eq, Generic, Hashable, Buildable)

-- | Client transaction id
newtype CTxId = CTxId CHash deriving (Show, Eq, Generic, Hashable)

-- | List of block entries is returned from "get latest N blocks" endpoint
data CBlockEntry = CBlockEntry
    { cbeHeight     :: !Word
    , cbeTimeIssued :: !POSIXTime
    , cbeTxNum      :: !Word
    , cbeTotalSent  :: !Coin
    , cbeSize       :: !Word64
    , cbeRelayedBy  :: !(Maybe Text)
    } deriving (Show, Generic)

-- | List of tx entries is returned from "get latest N transactions" endpoint
data CTxEntry = CTxEntry
    { cteId         :: !CTxId
    , cteTimeIssued :: !POSIXTime
    , cteAmount     :: !Coin
    } deriving (Show, Generic)
