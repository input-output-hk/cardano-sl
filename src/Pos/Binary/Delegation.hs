-- | Delegation types serialization.

module Pos.Binary.Delegation
       (
       ) where

import           Universum

import           Pos.Binary.Class     (Bi (..), label, labelS, putField)
import           Pos.Binary.Core      ()
import           Pos.Binary.Crypto    ()
import           Pos.Delegation.Types (DlgPayload (getDlgPayload), DlgUndo (..),
                                       mkDlgPayload)
import           Pos.Util.Util        (eitherToFail)

instance Bi DlgPayload where
    sizeNPut = labelS "DlgPayload" $ putField getDlgPayload
    get = label "DlgPayload" $ eitherToFail . mkDlgPayload =<< get

instance Bi DlgUndo where
    sizeNPut = labelS "DlgUndo" $ putField duPsks <> putField duPrevEpochPosted
    get = label "DlgUndo" $ DlgUndo <$> get <*> get
