module Cardano.Wallet.Kernel.DB.HdWallet.Compat
 ( hdRootsFromWalletStorage
 ) where

import Universum

import Control.Lens (review)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as T
import qualified Pos.Core as Core
import Pos.Crypto.Hashing (decodeAbstractHash)
import qualified Pos.Wallet.Web.ClientTypes as WebTypes
import qualified Pos.Wallet.Web.State.Storage as WS

import qualified Cardano.Wallet.Kernel.DB.HdWallet as Hdw
import Cardano.Wallet.Kernel.DB.InDb (InDb(InDb))

--------------------------------------------------------------------------------

hdRootsFromWalletStorage :: WS.WalletStorage -> Either String [Hdw.HdRoot]
hdRootsFromWalletStorage ws = do
  forM (HM.toList (WS._wsWalletInfos ws)) $ \(cwalId, wi) -> do
     let wMeta = WS._wiMeta wi :: WebTypes.CWalletMeta
     rId <- case hdRootIdFromCIdWal cwalId of
        Nothing -> Left "hdRootIdFromCIdWal"
        Just x -> pure x
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

-- | TODO IS THIS OK?
hdRootIdFromCIdWal :: WebTypes.CId WebTypes.Wal -> Maybe Hdw.HdRootId
hdRootIdFromCIdWal (WebTypes.CId (WebTypes.CHash t0)) = do
   bs0 <- decodeBase16 (T.encodeUtf8 t0)
   t1 <- either (const Nothing) Just (T.decodeUtf8' bs0)
   addrh <- either (const Nothing) Just (decodeAbstractHash t1)
   pure (Hdw.HdRootId (InDb addrh))

decodeBase16 :: B.ByteString -> Maybe B.ByteString
decodeBase16 = \x ->
   let (y, z) = Base16.decode x
   in if B.null z then Just y else Nothing

