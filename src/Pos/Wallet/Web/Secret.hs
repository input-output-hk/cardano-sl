{-# LANGUAGE TemplateHaskell #-}

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

import           Pos.Binary.Class    (Bi (..), label)
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

instance Bi WalletUserSecret where
    put WalletUserSecret{..} = do
        put _wusRootKey
        put _wusWalletName
        put _wusAccounts
        put _wusAddrs
    get = label "WalletUserSecret" $ do
        _wusRootKey <- get
        _wusWalletName <- get
        _wusAccounts <- get
        _wusAddrs <- get
        return WalletUserSecret{..}

mkGenesisWalletUserSecret :: EncryptedSecretKey -> WalletUserSecret
mkGenesisWalletUserSecret _wusRootKey = do
    let _wusWalletName = "Genesis wallet"
        _wusAccounts   = [(accountGenesisIndex, "Genesis account")]
        _wusAddrs      = [(accountGenesisIndex, wAddressGenesisIndex)]
    WalletUserSecret{..}
