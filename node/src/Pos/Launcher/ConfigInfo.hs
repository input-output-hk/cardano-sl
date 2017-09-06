module Pos.Launcher.ConfigInfo
       ( ConfigInfo(..)
       , applyConfigInfo
       ) where

import           Universum

import           Data.Default               (Default (..))
import           Data.Text.Buildable        (Buildable (..))
import qualified Data.Yaml                  as Y

import qualified Pos.Core.Genesis           as GenCore
import qualified Pos.Ssc.GodTossing.Genesis as GenGt
import qualified Pos.Util.Config            as Cfg

data ConfigInfo = ConfigInfo
    { customConfigPath  :: !(Maybe FilePath)  -- ^ constants.yaml
    , customConfigName  :: !(Maybe Text)      -- ^ section of the config
    , customGenCorePath :: !(Maybe FilePath)  -- ^ genesis-core.bin
    , customGenGtPath   :: !(Maybe FilePath)  -- ^ genesis-godtossing.bin
    }
    deriving (Eq, Show)

instance Default ConfigInfo where
    def = ConfigInfo Nothing Nothing Nothing Nothing

instance Buildable ConfigInfo where
    build ConfigInfo{..} = mconcat
        [ "{ config: "
        , maybe "embedded" build customConfigPath, "\n"
        , ", config name: "
        , maybe (build Cfg.defaultCslConfigName) build customConfigName, "\n"
        , ", genesis-core.bin: "
        , maybe "embedded" build customGenCorePath, "\n"
        , ", genesis-godtossing.bin: "
        , maybe "embedded" build customGenGtPath, " }"
        ]

-- | Initialize global vars with config and genesises.
applyConfigInfo :: ConfigInfo -> IO ()
applyConfigInfo ConfigInfo{..} = do
    case customConfigPath of
        Nothing   -> Cfg.setFullCslConfig Cfg.defaultFullCslConfig
        Just path -> Y.decodeFileEither @Y.Object path >>= \case
            Right x  -> Cfg.setFullCslConfig x
            Left err -> fail $ "Couldn't parse " ++ path ++ ": " ++
                               Y.prettyPrintParseException err
    case customConfigName of
        Nothing   -> Cfg.setCslConfigName Cfg.defaultCslConfigName
        Just name -> Cfg.setCslConfigName name
    case customGenCorePath of
        Nothing   -> GenCore.setGenCoreData GenCore.defaultGenCoreData
        Just path -> GenCore.setGenCoreDataFromFile path
    case customGenGtPath of
        Nothing   -> GenGt.setGenGtData GenGt.defaultGenGtData
        Just path -> GenGt.setGenGtDataFromFile path
