{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
module Bench.Pos.Remote.Config
       ( FullNodeConfig (..)
       , SupporterConfig (..)
       , readRemoteConfig
       ) where

import           Data.Aeson.TH          (deriveJSON)
import           Data.Aeson.Types       (FromJSON)
import qualified Data.Yaml              as Y
import           Serokell.Aeson.Options (defaultOptions)
import           Universum

data FullNodeConfig = FullNodeConfig
    { fncSupporterAddr :: !Text
    , fncPort          :: !Word16
    , fncDbPath        :: !(Maybe FilePath)
    , fncStartTime     :: !(Maybe Word64)
    }

data SupporterConfig = SupporterConfig
    { scPort   :: !Word16
    , scDHTKey :: !Text
    }

deriveJSON defaultOptions ''FullNodeConfig
deriveJSON defaultOptions ''SupporterConfig

readRemoteConfig :: FromJSON config => FilePath -> IO config
readRemoteConfig fp =
    either (panic . ("[FATAL] Failed to parse config: " <>) . show) identity <$>
    Y.decodeFileEither fp
