{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Pos.Util
       ( Raw

       , getCopyBinary
       , putCopyBinary
       ) where

import           Control.Monad.Fail      (fail)
import           Data.Binary             (Binary)
import qualified Data.Binary             as Binary (encode)
import           Data.Hashable           (Hashable)
import qualified Data.HashMap.Strict     as HM
import           Data.SafeCopy           (Contained, SafeCopy (..), contain, safeGet,
                                          safePut)
import qualified Data.Serialize          as Cereal (Get, Put)
import           Data.String             (String)
import           Universum

import           Serokell.Util.Binary    as Binary (decodeFull)

import           Pos.Util.NotImplemented ()

-- | A wrapper over 'ByteString' for adding type safety to
-- 'Pos.Crypto.Pki.encryptRaw' and friends.
newtype Raw = Raw ByteString
    deriving (Eq, Ord, Show)

-- | A helper for "Data.SafeCopy" that creates 'putCopy' given a 'Binary'
-- instance.
putCopyBinary :: Binary a => a -> Contained Cereal.Put
putCopyBinary x = contain $ safePut (Binary.encode x)

-- | A helper for "Data.SafeCopy" that creates 'getCopy' given a 'Binary'
-- instance.
getCopyBinary :: Binary a => String -> Contained (Cereal.Get a)
getCopyBinary typeName = contain $ do
    bs <- safeGet
    case Binary.decodeFull bs of
        Left err -> fail ("getCopy@" ++ typeName ++ ": " ++ err)
        Right x  -> return x

instance (Eq a, Hashable a, SafeCopy a, SafeCopy b) =>
         SafeCopy (HashMap a b) where
    getCopy = contain $ fmap HM.fromList safeGet
    putCopy = contain . safePut . HM.toList
