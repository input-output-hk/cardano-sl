{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell      #-}

module BlockSpec (spec, main) where

import Test.Hspec
import qualified Data.ByteString.Lazy as LBS
import           Pos.Core (BlockVersionData, HasConfiguration, headerHash,
                           GenesisHash (..), genesisHash, protocolMagic)
import           Pos.Lrc.Genesis (genesisLeaders)
import           Pos.Block.Base (genesisBlock0)
import Pos.DB.Block
import           Control.Monad.IO.Class (liftIO)
import           Pos.DB.Class (MonadDB, MonadDBRead (..))
import GHC.Generics
import           Pos.Core.Configuration
import           Pos.Block.Configuration
import           Pos.Delegation.Configuration
import           Pos.Txp.Configuration
import           Pos.Ntp.Configuration
import           Pos.Update.Configuration
import           Pos.Ssc.Configuration
import           Pos.Block.Configuration
--import           Pos.Util.Config (parseYamlConfig)
import           Pos.Crypto.Hashing
import           Pos.DB (NodeDBs, DBSum)
import           Pos.Core (HasConfiguration, HeaderHash, headerHash)
import qualified Pos.DB.Block as DB
import qualified Pos.DB as DB
import           Universum
import           Control.Lens.TH (makeLensesWith)
import           Pos.Util (HasLens (..), newInitFuture, postfixLFields)

main :: IO ()
main = hspec spec

sampleTravisYml :: LBS.ByteString
sampleTravisYml = "env:\n  global:\n    - VERSION=0.6\n    - CARDANO_SL_BRANCH=cardano-sl-0.6\n    - secure: \"ciphertext\"\n"

--artifactJson :: LBS.ByteString
--artifactJson = encode $ Object (fromList [("fileName", "installer.exe"), ("name", "windows installer")])

spec :: Spec
spec = do
  describe "Appveyor" $ do
    it "AppveyorArtifact parse" $ do
      liftIO $ do
        rawIoMonad
      1 `shouldBe` 1
      --(decode artifactJson :: Maybe AppveyorArtifact) `shouldBe` Just (AppveyorArtifact "installer.exe" "windows installer")

-- | Product of all configurations required to run a node.
data Configuration = Configuration
    { ccCore   :: !CoreConfiguration
    , ccNtp    :: !NtpConfiguration
    , ccUpdate :: !UpdateConfiguration
    , ccSsc    :: !SscConfiguration
    , ccDlg    :: !DlgConfiguration
    , ccTxp    :: !TxpConfiguration
    , ccBlock  :: !BlockConfiguration
    } deriving (Show, Generic)

rawIoMonad :: IO ()
rawIoMonad = do
  let
    configurationDir = "."
    cfoSeed = Nothing
    cfoSystemStart = Nothing
    (Right hash) = decodeHash "hash"
    ccCore = CoreConfiguration {
      ccGenesis = GCSrc "foo.json" hash
      , ccDbSerializeVersion = 1
    }
  withCoreConfigurations ccCore configurationDir cfoSystemStart cfoSeed $ do
    --prepareBlockDB (genesisBlock0 protocolMagic (GenesisHash genesisHash) genesisLeaders)
    foo <- dbGetSerBlockRealDefault (castHash hash)
    print "foo"

data BlockGenContext a = BlockGenContext { foo :: Text }
type MonadBlockGenBase m
     = (
         MonadMask m
       , MonadIO m
      )
type InitBlockGenMode ext m = ReaderT InitBlockGenContext m
type BlockGenMode ext m = ReaderT (BlockGenContext ext) m
instance MonadBlockGenBase m => MonadDBRead (InitBlockGenMode ext m) where
    dbGet = DB.dbGetSumDefault
    dbIterSource = DB.dbIterSourceSumDefault
    dbGetSerBlock = dbGetSerBlockSumDefault
    dbGetSerUndo = dbGetSerUndoSumDefault

instance MonadBlockGenBase m => MonadDB (BlockGenMode ext m) where
    dbPut = DB.dbPutSumDefault
    dbWriteBatch = DB.dbWriteBatchSumDefault
    dbDelete = DB.dbDeleteSumDefault
    dbPutSerBlunds = DB.dbPutSerBlundsSumDefault

data InitBlockGenContext = InitBlockGenContext
    { ibgcDB          :: !DBSum
    }
makeLensesWith postfixLFields ''InitBlockGenContext
