
module Pos.Binary.SafeCopy
       ( getCopyBi
       , putCopyBi
       ) where

import           Universum

import           Data.SafeCopy (Contained, contain, safeGet, safePut)
import qualified Data.Serialize as Cereal

import           Pos.Binary.Class (Bi)
import qualified Pos.Binary.Class as Bi

import           Pos.Util.Util (toCerealError)


getCopyBi :: forall a. Bi a => Contained (Cereal.Get a)
getCopyBi = contain $ do
    bs <- safeGet
    toCerealError $ case Bi.deserializeOrFail bs of
        Left (err, _) -> Left $ "getCopy@" <> Bi.label (Proxy @a) <> ": " <> show err
        Right (x, _)  -> Right x

putCopyBi :: Bi a => a -> Contained Cereal.Put
putCopyBi = contain . safePut . Bi.serialize
