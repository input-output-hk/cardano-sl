{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Miscellaneous unclassified utility functions.

module Pos.Util
       (
         module Pos.Util.Util
       -- * Stuff for testing and benchmarking
       , module Pos.Util.Arbitrary
       , module Pos.Util.TimeLimit

       -- * Various
       , mappendPair
       , mconcatPair
       , (<//>)
       , eitherToVerRes
       , readerToState
       , diffDoubleMap

       -- * NonEmpty
       , neZipWith3
       , spanSafe

       -- * Lenses
       , makeLensesData

       -- * Instances
       -- ** MonadFail ParsecT
       -- ** MonadFail Dialog
       -- ** MonadFail Transfer
       -- ** MonadFail TimedIO
       -- ** MonadFail ResponseT
       -- ** MonadFail LoggerNameBox
       ) where

import           Universum                     hiding (bracket, finally)

import           Control.Lens                  (lensRules)
import           Control.Lens.Internal.FieldTH (makeFieldOpticsForDec)
import qualified Control.Monad                 as Monad (fail)
import           Control.Monad.Trans.Resource  (ResourceT)
import           Data.Hashable                 (Hashable)
import qualified Data.HashMap.Strict           as HM
import           Data.List                     (span, zipWith3)
import qualified Data.Text                     as T
import qualified Language.Haskell.TH           as TH
import           Serokell.Util                 (VerificationRes (..))
import           System.Wlog                   (LoggerNameBox (..))
import           Text.Parsec                   (ParsecT)
-- SafeCopy instance for HashMap
import           Serokell.AcidState            ()

import           Pos.Util.Arbitrary
import           Pos.Util.TimeLimit
import           Pos.Util.Undefined            ()
import           Pos.Util.Util

-- | Specialized version of 'mappend' for restricted to pair type.
mappendPair :: (Monoid a, Monoid b) => (a, b) -> (a, b) -> (a, b)
mappendPair = mappend

-- | Specialized version of 'mconcat' (or 'Data.Foldable.fold')
-- for restricting type to list of pairs.
mconcatPair :: (Monoid a, Monoid b) => [(a, b)] -> (a, b)
mconcatPair = mconcat

-- | Concatenates two url part using regular slash '/'.
-- E.g. @"./dir/" <//> "/file" = "./dir/file"@.
(<//>) :: String -> String -> String
(<//>) lhs rhs = lhs' ++ "/" ++ rhs'
  where
    isSlash = (== '/')
    lhs' = reverse $ dropWhile isSlash $ reverse lhs
    rhs' = dropWhile isSlash rhs

-- | Convert (Reader s) to any (MonadState s)
readerToState
    :: MonadState s m
    => Reader s a -> m a
readerToState = gets . runReader

-- | Remove elements which are in 'b' from 'a'
diffDoubleMap
    :: forall k1 k2 v.
       (Eq k1, Eq k2, Hashable k1, Hashable k2)
    => HashMap k1 (HashMap k2 v)
    -> HashMap k1 (HashMap k2 v)
    -> HashMap k1 (HashMap k2 v)
diffDoubleMap a b = HM.foldlWithKey' go mempty a
  where
    go :: HashMap k1 (HashMap k2 v)
       -> k1
       -> HashMap k2 v
       -> HashMap k1 (HashMap k2 v)
    go res extKey internalMap =
        case HM.lookup extKey b of
            Nothing -> HM.insert extKey internalMap res
            Just internalMapB ->
                let diff = internalMap `HM.difference` internalMapB
                in if null diff
                       then res
                       else HM.insert extKey diff res

----------------------------------------------------------------------------
-- NonEmpty
----------------------------------------------------------------------------

neZipWith3 :: (x -> y -> z -> q) -> NonEmpty x -> NonEmpty y -> NonEmpty z -> NonEmpty q
neZipWith3 f (x :| xs) (y :| ys) (z :| zs) = f x y z :| zipWith3 f xs ys zs

-- | Makes a span on the list, considering tail only. Predicate has
-- list head as first argument. Used to take non-null prefix that
-- depends on the first element.
spanSafe :: (a -> a -> Bool) -> NonEmpty a -> (NonEmpty a, [a])
spanSafe p (x:|xs) = let (a,b) = span (p x) xs in (x:|a,b)

----------------------------------------------------------------------------
-- Lens utils
----------------------------------------------------------------------------

-- | Make lenses for a data family instance.
makeLensesData :: TH.Name -> TH.Name -> TH.DecsQ
makeLensesData familyName typeParamName = do
    info <- TH.reify familyName
    ins <- case info of
        TH.FamilyI _ ins -> return ins
        _                -> fail "makeLensesIndexed: expected data family name"
    typeParamInfo <- TH.reify typeParamName
    typeParam <- case typeParamInfo of
        TH.TyConI dec -> decToType dec
        _             -> fail "makeLensesIndexed: expected a type"
    let mbInsDec = find ((== Just typeParam) . getTypeParam) ins
    case mbInsDec of
        Nothing -> fail ("makeLensesIndexed: an instance for " ++
                         TH.nameBase typeParamName ++ " not found")
        Just insDec -> makeFieldOpticsForDec lensRules insDec
  where
    getTypeParam (TH.NewtypeInstD _ _ [t] _ _ _) = Just t
    getTypeParam (TH.DataInstD    _ _ [t] _ _ _) = Just t
    getTypeParam _                               = Nothing

    decToType (TH.DataD    _ n _ _ _ _) = return (TH.ConT n)
    decToType (TH.NewtypeD _ n _ _ _ _) = return (TH.ConT n)
    decToType other                     =
        fail ("makeLensesIndexed: decToType failed on: " ++ show other)

----------------------------------------------------------------------------
-- Deserialized wrapper
----------------------------------------------------------------------------

eitherToVerRes :: Either Text a -> VerificationRes
eitherToVerRes (Left errors) = if T.null errors then VerFailure []
                               else VerFailure $ T.split (==';') errors
eitherToVerRes (Right _ )    = VerSuccess


instance MonadFail (ParsecT s u m) where
    fail = Monad.fail

deriving instance MonadFail m => MonadFail (LoggerNameBox m)

instance MonadFail m => MonadFail (ResourceT m) where
    fail = lift . fail
