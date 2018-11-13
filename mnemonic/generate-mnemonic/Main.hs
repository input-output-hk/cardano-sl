{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

import           Universum

import           Cardano.Mnemonic (Mnemonic, entropyToMnemonic, genEntropy)
import           Data.Aeson (encode)
import           Data.ByteString.Lazy.Char8 (unpack)

main :: IO ()
main = do
    backupPhrase <- generateBackupPhrase
    let backupPhraseString = backupPhraseToString backupPhrase
    putStrLn backupPhraseString

backupPhraseToString :: Mnemonic 12 -> String
backupPhraseToString backupPhrase = unpack $ encode backupPhrase

generateBackupPhrase :: IO (Mnemonic 12)
generateBackupPhrase =
    entropyToMnemonic <$> genEntropy
