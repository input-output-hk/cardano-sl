{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Pos.Util
       ( Raw

       , getCopyBinary
       , putCopyBinary

       , makeLensesData
       ) where

import           Control.Lens                  (lensRules)
import           Control.Lens.Internal.FieldTH (makeFieldOpticsForDec)
import           Control.Monad.Fail            (fail)
import           Data.Binary                   (Binary)
import qualified Data.Binary                   as Binary (encode)
import           Data.Hashable                 (Hashable)
import qualified Data.HashMap.Strict           as HM
import           Data.SafeCopy                 (Contained, SafeCopy (..), contain,
                                                safeGet, safePut)
import qualified Data.Serialize                as Cereal (Get, Put)
import           Data.String                   (String)
import           Language.Haskell.TH
import           Universum

import           Serokell.Util.Binary          as Binary (decodeFull)

import           Pos.Util.NotImplemented       ()

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

----------------------------------------------------------------------------
-- Lens utils
----------------------------------------------------------------------------

-- | Make lenses for a data family instance.
makeLensesData :: Name -> Name -> DecsQ
makeLensesData familyName typeParamName = do
    info <- reify familyName
    ins <- case info of
        FamilyI _ ins -> return ins
        _             -> fail "makeLensesIndexed: expected data family name"
    typeParamInfo <- reify typeParamName
    typeParam <- case typeParamInfo of
        TyConI dec -> decToType dec
        _          -> fail "makeLensesIndexed: expected a type"
    let mbInsDec = find ((== Just typeParam) . getTypeParam) ins
    case mbInsDec of
        Nothing -> fail ("makeLensesIndexed: an instance for " ++
                         nameBase typeParamName ++ " not found")
        Just insDec -> makeFieldOpticsForDec lensRules insDec
  where
    getTypeParam (NewtypeInstD _ _ [t] _ _ _) = Just t
    getTypeParam (DataInstD    _ _ [t] _ _ _) = Just t
    getTypeParam _                            = Nothing

    decToType (DataD    _ n _ _ _ _) = return (ConT n)
    decToType (NewtypeD _ n _ _ _ _) = return (ConT n)
    decToType other                      =
        fail ("makeLensesIndexed: decToType failed on: " ++ show other)
