{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Main (main) where

import           Cardano.Crypto.Wallet (unXPrv)
import           Cardano.Mnemonic (Mnemonic, MnemonicError, mkMnemonic,
                     mnemonicToSeed)
import           Cardano.Wallet.Client.Http (BackupPhrase (BackupPhrase),
                     getAccIndex, unBackupPhrase)
import           Cardano.Wallet.Kernel.DB.HdWallet (HdRootId, eskToHdRootId,
                     getHdRootId, isOurs)
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.Decrypt (WalletDecrCredentials,
                     eskToWalletDecrCredentials)
import           Control.Lens (makeLensesWith)
import           Control.Monad.Reader ()
import qualified Data.ByteString as BS
import           Data.Conduit (mapOutputMaybe, runConduitRes, (.|))
import qualified Data.Conduit.List as Conduit
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import           Formatting (Format, bprint, fprint, hex, later, shown, (%), int, build)
import           Formatting.Internal.Raw (left)
import           Pos.Chain.Genesis (GenesisHash, configGenesisData, configGenesisHash)
import           Pos.Chain.Txp (TxIn, TxOutAux, toaOut, txOutAddress, txpTxs, txInputs,
                     txOutValue, Tx)
import           Pos.Core (Coin, IsBootstrapEraAddr (..), addrToBase58,
                     deriveLvl2KeyPair, mkCoin, unsafeAddCoin)
import           Pos.Core.NetworkMagic (NetworkMagic (..))
import           Pos.Crypto (ShouldCheckPassphrase (..), eskPayload,
                     safeDeterministicKeyGen)
import           Pos.DB (MonadDBRead (dbGet, dbGetSerBlock, dbGetSerBlund, dbGetSerUndo, dbIterSource),
                     NodeDBs, closeNodeDBs, dbGetDefault, dbIterSourceDefault,
                     openNodeDBs, getBlock)
import           Pos.DB.Block (dbGetSerBlockRealDefault, getFirstGenesisBlockHash, resolveForwardLink,
                     dbGetSerBlundRealDefault, dbGetSerUndoRealDefault)
import           Pos.DB.Txp.Utxo (utxoSource)
import           Pos.Util (HasLens (lensOf), postfixLFields)
import           System.Environment (lookupEnv)
import           Universum
import Pos.Launcher
import           Options.Applicative (Parser, auto, execParser, footerDoc, info, fullDesc)
import Pos.Client.CLI.Options
import Pos.Chain.Block
import           Pos.Util.Trace (Trace, fromTypeclassWlog, Severity(Info))
import           Data.Functor.Contravariant (contramap)
import           Pos.Util.Wlog.Compatibility (setupTestLogging)
import           Pos.Chain.Block (Block, Blund, HeaderHash, Undo, mainBlockSlot)
import qualified Data.List.NonEmpty as NE
import Pos.DB.Block
import qualified Formatting.Buildable as Buildable

data KeyToyContext = KeyToyContext { _ktNodeDBs :: NodeDBs }
makeLensesWith postfixLFields ''KeyToyContext

instance HasLens NodeDBs KeyToyContext NodeDBs where
    lensOf = _ktNodeDBs_L

hexString :: Format r (ByteString -> r)
hexString = later f
  where
    f :: ByteString -> T.Builder
    f bs = foldl' f2 "" $ BS.unpack bs
    f2 :: T.Builder -> Word8 -> T.Builder
    f2 str byte = str <> (left 2 '0' (bprint hex byte))

main :: IO ()
main = do
  let
  setupTestLogging
  putStrLn (""::String)

  cluster <- fromMaybe "mainnet" <$> lookupEnv "CLUSTER"
  let
    nm = case cluster of
      "mainnet" -> NetworkMainOrStage
      "staging" -> NetworkMainOrStage
      "testnet" -> NetworkTestnet 1097911063
      _         -> error "unsupported cluster"

  let
    ix = 0
    logTrace :: Trace IO (Severity, Text)
    logTrace = fromTypeclassWlog
    traceInfo :: Trace IO Text
    traceInfo = contramap ((,) Info) logTrace

  opts <- execParser $ info configurationOptionsParser fullDesc
  maybeDbPath <- lookupEnv "DB_PATH"
  print maybeDbPath
  case maybeDbPath of
    Nothing -> pure ()
    Just dbPath -> do
      bracket (openNodeDBs False dbPath) closeNodeDBs $ \db -> do
        withConfigurations traceInfo Nothing Nothing False opts $ \genesisConfig walletConfig txpConfig ntpConfig -> do
          let genesisHash = configGenesisHash genesisConfig
          runReaderT (iterateOverAllBlocks genesisHash) (KeyToyContext db)

type KeyToy = ReaderT KeyToyContext IO

instance MonadDBRead KeyToy where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault
    dbGetSerBlock = dbGetSerBlockRealDefault
    dbGetSerUndo = dbGetSerUndoRealDefault
    dbGetSerBlund = dbGetSerBlundRealDefault

iterateOverAllBlocks :: GenesisHash -> KeyToy ()
iterateOverAllBlocks genesisHash = do
  let
    go :: HeaderHash -> Int -> KeyToy ()
    go hh fatTxs = do
      blk <- getBlock genesisHash hh
      newCount <- case blk of
        Just (Right blk) -> do
          let
            txs = blk ^. mainBlockTxPayload . txpTxs
            go2 :: Int -> Tx -> KeyToy Int
            go2 count tx = do
              let
                s = length $ NE.toList (tx ^. txInputs)
              meos <- getHeaderEpochOrSlot hh
              if s > 300 then do
                liftIO $ fprint (build % " headerhash: " % shown % " count: " % int % "\n") meos hh s
                pure (count + 1)
              else
                pure count
          foldlM go2 fatTxs txs
        _ -> pure fatTxs
      nextHh <- resolveForwardLink hh
      case nextHh of
        Nothing -> do
          print fatTxs
          print "done"
        Just nextHh' -> go nextHh' newCount
  genhash <- getFirstGenesisBlockHash genesisHash
  go genhash 0
  pure ()
