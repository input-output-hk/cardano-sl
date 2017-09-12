-- | Contains part of 'Pos.Util.UserSecret' related to wallets

module Pos.Wallet.Web.Secret
    ( WalletUserSecret (..)
    , wusRootKey
    , wusWalletName
    , wusAccounts
    , wusAddrs
    , accountGenesisIndex
    , wAddressGenesisIndex

    , mkGenesisWalletUserSecret
    ) where

import           Universum

import           Control.Lens        (makeLenses)
import qualified Data.Text.Buildable
import           Formatting          (Format, bprint, build, later, (%))

import           Pos.Binary.Class    (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Core            (addressF, makeRootPubKeyAddress)
import           Pos.Crypto          (EncryptedSecretKey, encToPublic, firstHardened)

-- | First index in derivation path for HD account, which is put to genesis utxo
accountGenesisIndex :: Word32
accountGenesisIndex = firstHardened

-- | Second index in derivation path for HD account, which is put to genesis
-- utxo
wAddressGenesisIndex :: Word32
wAddressGenesisIndex = firstHardened

--- | Describes HD wallets keyfile content
data WalletUserSecret = WalletUserSecret
    { _wusRootKey    :: EncryptedSecretKey  -- ^ root key of wallet set
    , _wusWalletName :: Text                -- ^ name of wallet
    , _wusAccounts   :: [(Word32, Text)]    -- ^ accounts coordinates and names
    , _wusAddrs      :: [(Word32, Word32)]  -- ^ addresses coordinates
    }

makeLenses ''WalletUserSecret

instance Buildable WalletUserSecret where
    build WalletUserSecret{..} =
        bprint ("{ root = "%addressF%", set name = "%build%
                ", wallets = "%pairsF%", accounts = "%pairsF%" }")
        (makeRootPubKeyAddress $ encToPublic _wusRootKey)
        _wusWalletName
        _wusAccounts
        _wusAddrs
      where
        pairsF :: (Buildable a, Buildable b) => Format r ([(a, b)] -> r)
        pairsF = later $ mconcat . map (uncurry $ bprint ("("%build%", "%build%")"))

deriveSimpleBi ''WalletUserSecret [
    Cons 'WalletUserSecret [
        Field [| _wusRootKey    :: EncryptedSecretKey |],
        Field [| _wusWalletName :: Text               |],
        Field [| _wusAccounts   :: [(Word32, Text)]   |],
        Field [| _wusAddrs      :: [(Word32, Word32)] |]
    ]]

mkGenesisWalletUserSecret :: EncryptedSecretKey -> WalletUserSecret
mkGenesisWalletUserSecret _wusRootKey = do
    let _wusWalletName = "Genesis wallet"
        _wusAccounts   = [(accountGenesisIndex, "Genesis account")]
        _wusAddrs      = [(accountGenesisIndex, wAddressGenesisIndex)]
    WalletUserSecret{..}
