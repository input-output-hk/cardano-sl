module Pos.Wallet.Light.Launcher.Param
       ( WalletParams (..)
       ) where

import           Universum

import           Pos.Genesis  (GenesisContext)
import           Pos.Launcher (BaseParams)
import           Pos.Types    (Timestamp)

-- | Parameters that define wallet. Most of them are the same as in
-- "Pos.Launcher.Param", so take a look at 'NodeParams' if field is
-- undocumented here.
data WalletParams = WalletParams
    { wpDbPath          :: !(Maybe FilePath)
    , wpRebuildDb       :: !Bool
    , wpKeyFilePath     :: !FilePath
    , wpSystemStart     :: !Timestamp
    , wpGenesisKeys     :: !Bool
    , wpBaseParams      :: !BaseParams
    , wpGenesisContext  :: !GenesisContext
    , wpExplicitBootEra :: !Bool
      -- ^ If @True@, wallet thinks it's in boot era, in postboot era
      -- otherwise. Is needed since slotting data for lwallet is
      -- hardcoded.
    } deriving (Show)
