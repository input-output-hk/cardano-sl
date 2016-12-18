module Pos.Wallet.Launcher.Param
       ( WalletParams (..)
       ) where

import           Universum

import           Pos.Launcher (BaseParams)
import           Pos.Types    (Timestamp)

data WalletParams = WalletParams
    { wpDbPath      :: !(Maybe FilePath)
    , wpRebuildDb   :: !Bool
    , wpKeyFilePath :: !FilePath
    , wpSystemStart :: !Timestamp
    , wpGenesisKeys :: !Bool
    , wpBaseParams  :: !BaseParams
    } deriving (Show)
