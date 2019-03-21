{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

import Cardano.Mnemonic (mkMnemonic, MnemonicError, Mnemonic, mnemonicToSeed)
import Universum
import           Cardano.Wallet.Client.Http (BackupPhrase(BackupPhrase), unBackupPhrase, getAccIndex)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import           Pos.Crypto (ShouldCheckPassphrase (..), eskPayload, safeDeterministicKeyGen)
import           Formatting (Format, bprint, fprint, hex, later, shown, (%))
import qualified Data.ByteString as BS
import           Cardano.Crypto.Wallet (unXPrv)
import           Formatting.Internal.Raw (left)
import           Pos.Core (IsBootstrapEraAddr (..), deriveLvl2KeyPair, addrToBase58, Coin, unsafeAddCoin, mkCoin)
import           Pos.Core.NetworkMagic (NetworkMagic (..))
import           Cardano.Wallet.Kernel.DB.HdWallet (eskToHdRootId, getHdRootId, HdRootId, isOurs)
import           Cardano.Wallet.Kernel.Decrypt (WalletDecrCredentials, eskToWalletDecrCredentials)
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import System.Environment (lookupEnv)
import Pos.DB (MonadDBRead(dbGet,dbIterSource,dbGetSerBlock,dbGetSerUndo,dbGetSerBlund), openNodeDBs, closeNodeDBs, dbGetDefault, dbIterSourceDefault, NodeDBs)
import Control.Monad.Reader ()
import Pos.DB.Txp.Utxo (utxoSource)
import Pos.DB.Block (dbGetSerBlockRealDefault, dbGetSerUndoRealDefault, dbGetSerBlundRealDefault)
import           Control.Lens (makeLensesWith)
import           Pos.Util (postfixLFields, HasLens(lensOf))
import           Pos.Chain.Txp (TxIn, TxOutAux, toaOut, txOutAddress, txOutValue)
import qualified Data.Conduit.List as Conduit
import           Data.Conduit (mapOutputMaybe, runConduitRes, (.|))

data KeyToyContext = KeyToyContext { ktNodeDBs :: NodeDBs }
makeLensesWith postfixLFields ''KeyToyContext

instance HasLens NodeDBs KeyToyContext NodeDBs where
    lensOf = ktNodeDBs_L

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
    onError :: MnemonicError 4 -> IO BackupPhrase
    onError err = fail $ "Invalid BackupPhrase provided" <> show err
    onSuccess :: Mnemonic 12 -> IO BackupPhrase
    onSuccess = return . BackupPhrase
    phrase :: [Text] -> IO BackupPhrase
    phrase ws = either onError onSuccess (mkMnemonic ws)
  args <- getArgs
  mnemonic <- phrase (map T.pack args)
  print args
  let (thing, esk) = safeDeterministicKeyGen (mnemonicToSeed $ unBackupPhrase mnemonic) mempty
  print thing
  print esk
  let
    (epriv,remain) = BS.splitAt 64 $ unXPrv $ eskPayload esk
    (pub,chaincode) = BS.splitAt 32 remain
  fprint ("XPrv: " % hexString % ", pub: " % hexString % ", CC: " % hexString % "\n") epriv pub chaincode
  putStrLn (""::String)

  cluster <- fromMaybe "mainnet" <$> lookupEnv "CLUSTER"
  let
    nm = case cluster of
      "mainnet" -> NetworkMainOrStage
      "staging" -> NetworkMainOrStage
      "testnet" -> NetworkTestnet 1097911063
      _ -> error "unsupported cluster"
    rootid = eskToHdRootId nm esk
    walletid = addrToBase58 $ view fromDb (getHdRootId rootid)
    wdc = eskToWalletDecrCredentials nm esk

  let
    ix = 0
    maddr = fst <$> deriveLvl2KeyPair nm (IsBootstrapEraAddr True) (ShouldCheckPassphrase False) mempty esk (getAccIndex minBound) ix

  case maddr of
    Nothing -> fail "The impossible happened: failed to generate a\                                           \ random address. This can only happened if you\
           \ provided a derivation index that is out-of-bound!"
    Just addr -> do
      fprint ("walletid: " % shown % ", addr: " % shown % ": " % shown % "\n") walletid cluster (addrToBase58 addr)

  maybeDbPath <- lookupEnv "DB_PATH"
  print maybeDbPath
  case maybeDbPath of
    Nothing -> pure ()
    Just dbPath -> do
      bracket (openNodeDBs False dbPath) closeNodeDBs $ \db -> do
        balance <- runReaderT (queryWalletBalance rootid wdc) (KeyToyContext db)
        print balance

type KeyToy = ReaderT KeyToyContext IO

instance MonadDBRead KeyToy where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault
    dbGetSerBlock = dbGetSerBlockRealDefault
    dbGetSerUndo = dbGetSerUndoRealDefault
    dbGetSerBlund = dbGetSerBlundRealDefault

-- takes a WalletDecrCredentials and transaction, and returns the Coin output, if its ours
maybeReadcoin :: (HdRootId, WalletDecrCredentials) -> (TxIn, TxOutAux) -> Maybe Coin
maybeReadcoin wkey (_, txout) = case isOurs (txOutAddress . toaOut $ txout) [wkey] of
  (Just _, _)  -> Just $ (txOutValue . toaOut) txout
  (Nothing, _)-> Nothing

filterUtxo :: ((TxIn, TxOutAux) -> Maybe a) -> KeyToy [a]
filterUtxo p = runConduitRes $ mapOutputMaybe p utxoSource .| Conduit.fold (flip (:)) []

queryWalletBalance :: HdRootId -> WalletDecrCredentials -> KeyToy Coin
queryWalletBalance rootid wdc = do
  my_coins <- filterUtxo (maybeReadcoin (rootid, wdc))
  let
    balance :: Coin
    balance = foldl' unsafeAddCoin (mkCoin 0) my_coins
  pure balance
