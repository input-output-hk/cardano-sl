#!/usr/bin/env stack
-- stack script --resolver lts-9.1

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

import           Control.Lens
import           Control.Monad (forM_, void, when)
import           Control.Monad.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans (lift)
import           "lens-aeson" Data.Aeson.Lens
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromJust, fromMaybe)
import           Data.Monoid ((<>))
import           Data.Scientific (Scientific)
import qualified Data.Text as T
import           Data.Text.IO as TIO
import           Data.Text.Lazy (fromStrict)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Yaml as Y
import           Filesystem.Path.CurrentOS (encodeString)
import qualified Filesystem.Path.CurrentOS as F
import           GHC.Generics
import           Prelude hiding (FilePath)
import           Text.Printf
import           Turtle hiding (printf)


-- CLI Parser

data Command =
    Update
  | GenWalletConf !FilePath
  deriving (Show)

cmdParser :: Parser Command
cmdParser =
      subcommand "update" "Test software update" (pure Update)
  <|> subcommand "gen-wallet-conf" "generate wallet configuration"
        (GenWalletConf <$> optPath "out" 'o' "Output file")

data Config = Config
      { cFile :: FilePath
      , cKey  :: Text
      , cCmd  :: Command
      }

parser :: Parser Config
parser = Config
    <$> optPath "configuration-file" 'c' "Configuration file"
    <*> optText "configuration-key" 'k' "Configuration key"
    <*> cmdParser

walletAppName :: Text
walletAppName = "csl-daedalus"

main :: IO ()
main = do
  c@Config {..} <- options "Helper CLI around NixOps to run experiments" parser
  config <- fromMaybe (error "No config key found") . M.lookup cKey . either (error . show) id
              <$> Y.decodeFileEither (F.encodeString cFile)
  case cCmd of
    Update -> updateTest config c
    GenWalletConf outFile ->
        let config' = config & appNameL .~ walletAppName
         in Y.encodeFile (F.encodeString outFile) (M.singleton cKey config')


genSpecL :: AsValue t => Traversal' t Y.Value
genSpecL = key "core"
         . key "genesis"
         . key "spec"

genesisConstL = genSpecL
         . key "blockVersionData"

genSlotL = genesisConstL
         . key "slotDuration"
         . _Number

genBlockSizeL = genesisConstL
         . key "maxBlockSize"
         . _Number

genScriptVersionL = genesisConstL
         . key "scriptVersion"
         . _Number

richL :: AsValue t => Traversal' t Scientific
richL = genSpecL
         . key "initializer"
         . key "testBalance"
         . key "richmen"
         . _Number

svL :: AsValue t => Traversal' t Scientific
svL = key "update"
         . key "applicationVersion"
         . _Number

tagL = key "update"
         . key "systemTag"
         . _String

appNameL :: AsValue t => Traversal' t Text
appNameL = key "update"
         . key "applicationName"
         . _String

bvL = key "update" . key "lastKnownBlockVersion"

bvAltL = bvL . key "bvAlt" . _Number
bvMinorL = bvL . key "bvMinor" . _Number
bvMajorL = bvL . key "bvMajor" . _Number

updateTest :: Y.Value -> Config -> IO ()
updateTest config (Config{..}) = do
    let config' = config & appNameL .~ walletAppName & svL +~ 1
        showF = T.pack . show @Int . round
        cFile' = T.pack $ F.encodeString cFile
        (proposeUpdStr, cNodes) = fromMaybe (error "Failed to form update proposal cmd") $ do
            tag <- config ^? tagL
            bvAlt <- config ^? bvAltL
            bvMinor <- config ^? bvMinorL
            bvMajor <- config ^? bvMajorL
            genSlot <- config ^? genSlotL
            genScriptVersion <- config ^? genScriptVersionL
            genBlockSize <- config ^? genBlockSizeL
            sv <- config ^? svL
            (cNodes :: Int) <- round <$> config ^? richL
            let bvmStr = "bvm"
                    <> " script-version: " <> showF genScriptVersion
                    <> " slot-duration: " <> showF (genSlot / 1000)
                    <> " max-block-size: " <> showF genBlockSize
                updStr = "upd-bin"
                    <> " system: " <> T.pack (show tag)
                    <> " installer-path: ./test_updater.sh"
            return $ (,cNodes) $ "propose-update i:0"
              <> " block-version: " <> showF bvMajor <> "." <> showF bvMinor <> "." <> showF bvAlt
              <> " software-version: ~software~" <> walletAppName <> ":" <> showF (sv + 1)
              <> " bvm: (" <> bvmStr <> ")"
              <> " update: (" <> updStr <> ")"
              <> " vote-all: true"
    TIO.putStrLn proposeUpdStr
    Y.encodeFile "run/conf-fresh.yaml" (M.singleton cKey config')

    TIO.writeFile "test_updater.sh" $ "cp run/conf-fresh.yaml run/configuration.wallet.yaml"
    let cmdPropose = T.intercalate ";"
            [ "add-key-pool " <> T.unwords (format ("i:"%d) <$> [0..cNodes-1])
            , "listaddr"
            , proposeUpdStr
            ]
        auxx cmds = ("cardano-auxx",)
            [ "--keyfile", "run/auxx.keys"
            , "--configuration-key", cKey
            , "--configuration-file", cFile'
            , "--system-start", "0"
            , "--peer", "127.0.0.1:3001"
            , "--db-path", "run/auxx-db"
            , "--log-config", "scripts/log-templates/log-config-greppable.yaml"
            , "cmd", "--commands", cmds
            ]
    (exitCode, textToLines -> auxxOut) <- uncurry procStrict (auxx cmdPropose) empty
    case exitCode of
        ExitSuccess -> do
            echo "Proposing an update succeeded, here are the logs:"
            mapM_ (echo . mappend "\t") auxxOut
        ExitFailure _ -> do
            echo "Proposing an update failed, see auxx output:"
            mapM_ (echo . mappend "\t") auxxOut
            exit exitCode
    let matchInAuxxOut pat = do
            t <- NE.toList auxxOut
            match (suffix pat) (lineToText t)
        extractFromAuxxOut s pat =
            case matchInAuxxOut pat of
                m:_ -> return m
                _ -> do
                    echo $ "Could not extract " <> s <> " from auxxOut"
                    exit $ ExitFailure 1
    fileHash <- extractFromAuxxOut "fileHash" $
        "Read file ./test_updater.sh succesfuly, its hash: " *>
        (T.pack <$> some hexDigit)
    updHash <- extractFromAuxxOut "updHash" $
        "Update proposal submitted along with votes, upId: " *>
        (T.pack <$> some hexDigit)
    TIO.putStrLn $ fileHash <> " " <> updHash

    shells ("mkdir run/serve-upd; mv test_updater.sh run/serve-upd/"<>fileHash) empty
