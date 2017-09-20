module Pos.Wallet.Light.Launcher.Param
       ( WalletParams (..)
       ) where

import           Universum

import           Pos.Launcher (BaseParams)

data WalletParams = WalletParams
    { wpDbPath         :: !(Maybe FilePath)
    , wpRebuildDb      :: !Bool
    , wpKeyFilePath    :: !FilePath
    , wpGenesisKeys    :: !Bool
    , wpBaseParams     :: !BaseParams
    } deriving (Show)
