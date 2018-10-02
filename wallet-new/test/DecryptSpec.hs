module DecryptSpec where

import           Universum

import           Test.Hspec

import           Cardano.Crypto.Wallet (ChainCode (..), XPub (..))
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel.Decrypt as V1
import           Pos.Crypto (PublicKey (..))

spec :: Spec
spec = describe "Decrypt" $ do
    describe "credentialsFromPublicKey" $ do
        it "works with example key" $
            let key = PublicKey $ XPub { xpubPublicKey = "\SI\SO\241<\240\226hZ^w53\245\EOT\248)\190\200\234f\EOTN\241\197D\t\245\ETX\158, \177"
                                       , xpubChaincode = ChainCode "\DLE\139\178\183\ACKE\166e\f\242Xr\a\168\187\RS\166\190~\246\t\154\255)\150\DC1}\197\236\217\243{"
                                       }
            in (snd . V1.keyToWalletDecrCredentials $ V1.KeyForExternal key)
               `shouldBe`
               (V1.WalletId "Ae2tdPwUPEZ7N9manByhCNSvPN7s3cAxTumkjnQRW2vSpfmkwqrf6D84DWq")
