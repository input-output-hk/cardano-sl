{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Cardano.Wallet.API.V1.TH
       ( conNamesList
       , deriveWalletErrorJSON
       ) where

import           Universum

import           Data.Aeson
import           Data.Char (isUpper, toLower)
import qualified Data.List as DL
import           Language.Haskell.TH
import qualified Serokell.Aeson.Options as Serokell

--
-- String manipulation utils
--

headToLower :: String -> String
headToLower []     = []
headToLower (x:xs) = toLower x : xs

stripFieldPrefix :: String -> String
stripFieldPrefix = dropWhile (not . isUpper)

mkJsonKey :: String -> String
mkJsonKey s =
    let s' = headToLower $ stripFieldPrefix s
    in if null s' then s else s'

--
-- Utils
--

-- | Fetch constructor's name from its definition
conName :: Con -> Name
conName (NormalC name _)     = name
conName (RecC name _)        = name
conName (InfixC _ name _)    = name
conName (ForallC _ _ con)    = conName con
conName (GadtC names _ _)    = DL.head names
conName (RecGadtC names _ _) = DL.head names

-- -- | Gets a name from `TyVarBndr`
tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV name)    = name
tyVarBndrName (KindedTV name _) = name

-- | Constructs type definition from given type `Name` and list of `TyVarBndr`s
-- Example (for `Either a b` datatype):
--
-- Name: `Either`
-- Var binders: `[PlainTV a, PlainTV b]`
-- Resulting type: `AppT (AppT (ConT Either) (VarT a)) (VarT b)`
mkTypeFromNameAndVars :: Name -> [TyVarBndr] -> Type
mkTypeFromNameAndVars name = foldl AppT (ConT name) . map (VarT . tyVarBndrName)

-- | Make a constraint from a typeclass name and a `Type`
mkAppConstraint :: Name -> Type -> Type
mkAppConstraint tClass = AppT (ConT tClass)

-- | Make a list of `AppT` constraints from given typeclass' name and list of variables
mkAppConstraintsFromVars :: Name -> [TyVarBndr] -> Cxt
mkAppConstraintsFromVars tClass = map (mkAppConstraint tClass . VarT . tyVarBndrName)

-- | Reify a typename and report an error if provided name is not a valid typename
reifyTypename :: Name -> Q Dec
reifyTypename = reify >=> \case
    TyConI dec -> pure dec
    _otherwise -> fail "reifyTypename: provided name is not a typename"

-- | Produce a string literal which contains string representation of `Name`
nameToStringL :: Name -> ExpQ
nameToStringL = litE . stringL . nameBase

-- | Produce a list of string representation of `Name`s
namesList :: [Name] -> ExpQ
namesList = listE . map nameToStringL

-- | Produce a list with string representations of constructor names of given type
conNamesList :: Name -> ExpQ
conNamesList = reifyTypename >=> \case
    DataD _ _ _ _ cons _   -> namesList $ map conName cons
    NewtypeD _ _ _ _ con _ -> namesList [conName con]
    _otherwise             -> fail "conNamesList: type name should refer to `data` or `newtype`"

--
-- Derivation of JSON instances for wallet errors
--


-- | Derive both 'ToJSON' and 'FromJSON' instances for `WalletError`.
deriveWalletErrorJSON :: Name -> DecsQ
deriveWalletErrorJSON = reifyTypename >=> \case
    DataD _ name typeVars _ cons _ -> do
        toJson <- deriveWalletErrorToJSON name typeVars cons
        fromJson <- pure [] -- TODO: provide `FromJSON instance`
        pure $ toJson ++ fromJson
    _otherwise -> fail "deriveWalletErrorJSON: type name should refer to `data`"

-- | Derive only `ToJSON` instance for `WalletError`
deriveWalletErrorToJSON :: Name -> [TyVarBndr] -> [Con] -> DecsQ
deriveWalletErrorToJSON name typeVars cons = pure <$> instanceD
    (pure $ mkAppConstraintsFromVars ''ToJSON typeVars)
    (pure $ mkAppConstraint ''ToJSON $ mkTypeFromNameAndVars name typeVars)
    [funD 'toJSON $ map mkToJsonClause cons]

-- | Make body of `toJSON` function for given constructor
mkToJsonClause :: Con -> ClauseQ
mkToJsonClause (NormalC name vars) =
    mkToJsonClauseGeneric name $ map (("v"++) . show) [1 .. length vars]
mkToJsonClause (RecC name vars) =
    mkToJsonClauseGeneric name $ map (nameBase . view _1) vars
mkToJsonClause _ = fail "mkToJsonClause: unsupported constructor type"

-- | Make body of `toJSON` function. Accept constructor name and
-- constructor variable names.
mkToJsonClauseGeneric :: Name -> [String] -> ClauseQ
mkToJsonClauseGeneric constrName varNames = do
    bindings <- forM varNames $ \name' ->
        (name',) <$> newName name'

    let pats = map (varP . snd) bindings
        mkVarAssignment (name, bind) = [e| $(stringE $ mkJsonKey name) .= $(varE bind) |]

    clause
        [conP constrName pats]
        (normalB
            [e| object [ "message" .= String $(stringE $ nameBase constrName)
                       , "diagnostic" .= object $(listE $ map mkVarAssignment bindings)
                       ]
              |])
        []

-- deriveWalletErrorFromJSON :: Name -> [TyVarBndr] -> [Con] -> DecsQ
-- deriveWalletErrorFromJSON = undefined
