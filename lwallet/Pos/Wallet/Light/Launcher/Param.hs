module Pos.Wallet.Light.Launcher.Param
       ( WalletParams (..)
       ) where

import           Universum

import           Pos.Launcher (BaseParams)
import           Pos.Txp      (GenesisUtxo)
import           Pos.Types    (Timestamp)

data WalletParams = WalletParams
    { wpDbPath      :: !(Maybe FilePath)
    , wpRebuildDb   :: !Bool
    , wpKeyFilePath :: !FilePath
    , wpSystemStart :: Timestamp  -- System start is not defined now
    , wpGenesisKeys :: !Bool
    , wpBaseParams  :: !BaseParams
    , wpGenesisUtxo :: !GenesisUtxo -- ^ Genesis utxo
    } deriving (Show)
