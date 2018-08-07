#!/usr/bin/env nix-shell
#!nix-shell -i runhaskell ../wallet-new/default.nix
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
import           Data.Aeson (encode)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Pos.Util.Mnemonic (EntropySize, Mnemonic, entropyToMnemonic,
                     genEntropy)

main = do
    backupPhrase <- generateBackupPhrase
    let backupPhraseString = backupPhraseToString backupPhrase
    putStrLn backupPhraseString

backupPhraseToString :: Mnemonic 12 -> String
backupPhraseToString backupPhrase = unpack $ encode backupPhrase

generateBackupPhrase :: IO (Mnemonic 12)
generateBackupPhrase =
    entropyToMnemonic <$> genEntropy
