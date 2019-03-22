{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

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
import           Formatting (Format, bprint, fprint, hex, later, shown, (%))
import           Formatting.Internal.Raw (left)
import           Pos.Chain.Txp (TxIn, TxOutAux, toaOut, txOutAddress,
                     txOutValue)
import           Pos.Core (Coin, IsBootstrapEraAddr (..), addrToBase58,
                     deriveLvl2KeyPair, mkCoin, unsafeAddCoin)
import           Pos.Core.NetworkMagic (NetworkMagic (..))
import           Pos.Crypto (ShouldCheckPassphrase (..), eskPayload,
                     safeDeterministicKeyGen)
import           Pos.DB (MonadDBRead (dbGet, dbGetSerBlock, dbGetSerBlund, dbGetSerUndo, dbIterSource),
                     NodeDBs, closeNodeDBs, dbGetDefault, dbIterSourceDefault,
                     openNodeDBs)
import           Pos.DB.Block (dbGetSerBlockRealDefault,
                     dbGetSerBlundRealDefault, dbGetSerUndoRealDefault)
import           Pos.DB.Txp.Utxo (utxoSource)
import           Pos.Util (HasLens (lensOf), postfixLFields)
import           System.Environment (lookupEnv)
import           Universum

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
      _         -> error "unsupported cluster"
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
