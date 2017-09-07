module Pos.Rubbish.Param
       ( WalletParams (..)
       ) where

import           Universum

import           Pos.Core     (Timestamp)
import           Pos.Genesis  (GenesisContext)
import           Pos.Launcher (BaseParams)

data WalletParams = WalletParams
    { wpDbPath         :: !(Maybe FilePath)
    , wpRebuildDb      :: !Bool
    , wpKeyFilePath    :: !FilePath
    , wpSystemStart    :: Timestamp  -- System start is not defined now
    , wpGenesisKeys    :: !Bool
    , wpBaseParams     :: !BaseParams
    , wpGenesisContext :: !GenesisContext
    } deriving (Show)
