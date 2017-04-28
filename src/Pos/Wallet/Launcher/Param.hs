module Pos.Wallet.Launcher.Param
       ( WalletParams (..)
       ) where

import           Universum

import           Pos.Launcher (BaseParams)
import           Pos.Txp      (Utxo)
import           Pos.Types    (Timestamp)

data WalletParams = WalletParams
    { wpDbPath      :: !(Maybe FilePath)
    , wpRebuildDb   :: !Bool
    , wpKeyFilePath :: !FilePath
    , wpSystemStart :: Timestamp  -- System start is not defined now
    , wpGenesisKeys :: !Bool
    , wpBaseParams  :: !BaseParams
    , wpGenesisUtxo :: !Utxo -- ^ Genesis utxo
    , wpJLFilePath  :: !(Maybe FilePath) -- ^ JSON log file path
    } deriving (Show)
