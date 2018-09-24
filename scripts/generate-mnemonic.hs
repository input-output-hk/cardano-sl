#!/usr/bin/env nix-shell
#!nix-shell -i runhaskell ../wallet-new/default.nix
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
import           Cardano.Wallet.Kernel.BIP39 (EntropySize, Mnemonic,
                     entropyToMnemonic, genEntropy)
import           Data.Aeson (encode)
import           Data.ByteString.Lazy.Char8 (unpack)

main = do
    backupPhrase <- generateBackupPhrase
    let backupPhraseString = backupPhraseToString backupPhrase
    putStrLn backupPhraseString

backupPhraseToString :: Mnemonic 12 -> String
backupPhraseToString backupPhrase = unpack $ encode backupPhrase

generateBackupPhrase :: IO (Mnemonic 12)
generateBackupPhrase =
    entropyToMnemonic <$> genEntropy
