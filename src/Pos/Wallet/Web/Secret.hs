-- | Contains part of 'Pos.Util.UserSecret' related to wallets

module Pos.Wallet.Web.Secret
    ( WalletUserSecret (..)
    , wusRootKey
    , wusWalletName
    , wusAccounts
    , wusAddrs

    , mkGenesisWalletUserSecret
    ) where

import           Control.Lens        (makeLenses)
import qualified Data.Text.Buildable
import           Formatting          (Format, bprint, build, later, (%))
import           Universum

import           Pos.Binary.Class    (Cons (..), Field (..), deriveSimpleBi)
import qualified Pos.Binary.Cbor     as Cbor
import           Pos.Crypto          (EncryptedSecretKey, encToPublic)
import           Pos.Genesis         (accountGenesisIndex, wAddressGenesisIndex)
import           Pos.Types           (addressF, makePubKeyAddress)

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
        (makePubKeyAddress $ encToPublic _wusRootKey)
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

Cbor.deriveSimpleBi ''WalletUserSecret [
    Cbor.Cons 'WalletUserSecret [
        Cbor.Field [| _wusRootKey    :: EncryptedSecretKey |],
        Cbor.Field [| _wusWalletName :: Text               |],
        Cbor.Field [| _wusAccounts   :: [(Word32, Text)]   |],
        Cbor.Field [| _wusAddrs      :: [(Word32, Word32)] |]
    ]]

mkGenesisWalletUserSecret :: EncryptedSecretKey -> WalletUserSecret
mkGenesisWalletUserSecret _wusRootKey = do
    let _wusWalletName = "Genesis wallet"
        _wusAccounts   = [(accountGenesisIndex, "Genesis account")]
        _wusAddrs      = [(accountGenesisIndex, wAddressGenesisIndex)]
    WalletUserSecret{..}
