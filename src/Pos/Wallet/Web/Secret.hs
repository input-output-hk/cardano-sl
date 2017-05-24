-- | Contains part of 'Pos.Util.UserSecret' related to wallets

module Pos.Wallet.Web.Secret
  ( WalletUserSecret (..)
  ) where

import qualified Data.Text.Buildable
import           Formatting          (Format, bprint, build, later, (%))
import           Universum

import           Pos.Binary.Class    (Bi (..), label)
import           Pos.Crypto          (EncryptedSecretKey, encToPublic)
import           Pos.Types           (addressF, makePubKeyAddress)

--- | Describes HD wallets keyfile content
data WalletUserSecret = WalletUserSecret
    { wusRootKey  :: EncryptedSecretKey  -- ^ root key of wallet set
    , wusWSetName :: Text                -- ^ name of wallet set
    , wusWallets  :: [(Word32, Text)]    -- ^ coordinates and names wallets
    , wusAccounts :: [(Word32, Word32)]  -- ^ coordinates of accounts
    }

instance Buildable WalletUserSecret where
    build WalletUserSecret{..} =
        bprint ("{ root = "%addressF%", set name = "%build%
                ", wallets = "%pairsF%", accounts = "%pairsF%" }")
        (makePubKeyAddress $ encToPublic wusRootKey)
        wusWSetName
        wusWallets
        wusAccounts
      where
        pairsF :: (Buildable a, Buildable b) => Format r ([(a, b)] -> r)
        pairsF = later $ mconcat . map (uncurry $ bprint ("("%build%", "%build%")"))

instance Bi WalletUserSecret where
    put WalletUserSecret{..} = do
        put wusRootKey
        put wusWSetName
        put wusWallets
        put wusAccounts
    get = label "WalletUserSecret" $ do
        wusRootKey <- get
        wusWSetName <- get
        wusWallets <- get
        wusAccounts <- get
        return WalletUserSecret{..}
