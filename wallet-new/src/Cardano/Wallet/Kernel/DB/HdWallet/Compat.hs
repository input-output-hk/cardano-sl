module Cardano.Wallet.Kernel.DB.HdWallet.Compat
 ( hdRootsFromWalletStorage
 , cIdWalToHashPublicKey
 , cIdWalToHdRootId
 ) where

import Universum

import Control.Lens (review)
import Control.Monad (guard)
import Crypto.Hash (Blake2b_224, digestFromByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as T

import qualified Pos.Core as Core
import Pos.Crypto (AbstractHash(AbstractHash), PublicKey)
import Pos.Crypto.Hashing (decodeAbstractHash)
import qualified Pos.Wallet.Web.ClientTypes as WebTypes
import qualified Pos.Wallet.Web.State.Storage as WS

import qualified Cardano.Wallet.Kernel.DB.HdWallet as Hdw
import Cardano.Wallet.Kernel.DB.InDb (InDb(InDb))

--------------------------------------------------------------------------------

-- | Obtain all of the 'Hdw.HdRoot's in the 'WS.WalletStorage'.
hdRootsFromWalletStorage :: WS.WalletStorage -> Either String [Hdw.HdRoot]
hdRootsFromWalletStorage ws = do
  forM (HM.toList (WS._wsWalletInfos ws)) $ \(cwalId, wi) -> do
     let wMeta = WS._wiMeta wi :: WebTypes.CWalletMeta
     rId <- case cIdWalToHdRootId cwalId of
        Nothing -> Left "cIdToHdRootId: bad 'CId Wal'"
        Just x -> pure (Hdw.HdRootId (InDb x))
     pure (Hdw.HdRoot
        { Hdw._hdRootId = rId
        , Hdw._hdRootName = Hdw.WalletName (WebTypes.cwName wMeta)
        , Hdw._hdRootHasPassword = Hdw.HasSpendingPassword
            (InDb (review Core.timestampSeconds (WS._wiPassphraseLU wi)))
        , Hdw._hdRootAssurance = case WebTypes.cwAssurance wMeta of
            WebTypes.CWAStrict -> Hdw.AssuranceLevelStrict
            WebTypes.CWANormal -> Hdw.AssuranceLevelNormal
        , Hdw._hdRootCreatedAt =
            InDb (review Core.timestampSeconds (WS._wiCreationTime wi))
        })

cIdWalToHashPublicKey :: CId Wal -> Maybe (AbstractHash Blake2b_224 PublicKey)
cIdWalToHashPublicKey (WebTypes.CId (WebTypes.CHash t0)) = do
   bs0 <- decodeBase16 (T.encodeUtf8 t0)
   dig <- digestFromByteString bs0
   pure (AbstractHash dig)

cIdWalToHdRootId :: CId Wal -> Maybe Hdw.HdRootId
cIdWalToHdRootId = fmap (Hdw.HdRootId . InDb) . cIdWalToHashPublicKey

decodeBase16 :: B.ByteString -> Maybe B.ByteString
decodeBase16 = \x -> do
   let (y, z) = Base16.decode x
   guard (B.null z)
   pure y
