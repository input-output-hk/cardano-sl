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
import           Data.Aeson.Types (Parser)
import qualified Data.List as DL
import           Language.Haskell.TH
import qualified Serokell.Aeson.Options as Serokell

import           Cardano.Wallet.Util (mkJsonKey)

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
-- NB: Unfortunately, `Options` from `Data.Aeson.TH` are not flexible
-- enough to generate JSON instances in the decided way (see `Cardano.Wallet.API.V1.Errors`
-- for details) using standard `deriveJSON` function.
--
-- If we could put additional error data not in separate `diagnostic` object,
-- but in root object along with error tag, e. g.
-- ```
-- {
--     "message" : "SomeOtherError",
--     "foo" : "blah",
--     "bar" : 3
-- }
-- ```
-- then it would be possible to derive JSON instances using `deriveJSON`,
-- which would make this file way shorter.

-- | Derive both 'ToJSON' and 'FromJSON' instances for `WalletError`.
deriveWalletErrorJSON :: Name -> DecsQ
deriveWalletErrorJSON = reifyTypename >=> \case
    DataD _ name typeVars _ cons _ -> do
        toJson <- deriveWalletErrorToJSON name typeVars cons
        fromJson <- deriveWalletErrorFromJSON name typeVars cons
        pure $ toJson ++ fromJson
    _otherwise -> fail "deriveWalletErrorJSON: type name should refer to `data`"

-- | Get wallet error name and variable names from a constructor.
-- Fails, if given constructor type is not supported.
getErrNameAndVars :: Con -> Q (Name, [String])
getErrNameAndVars (NormalC name vars)
    | null vars = pure (name, [])
    | otherwise = fail "getErrNameAndVars: non-record constructor fields are not supported"
getErrNameAndVars (RecC name vars) =
    pure (name, map (nameBase . view _1) vars)
getErrNameAndVars _ = fail "getErrNameAndVars: unsupported constructor type"

-- | Make bindings for var names
mkBindings :: [String] -> Q [(String, Name)]
mkBindings = mapM (\name -> (name,) <$> newName name)

--
-- ToJSON instance derivation
--

-- | Derive only `ToJSON` instance for `WalletError`
deriveWalletErrorToJSON :: Name -> [TyVarBndr] -> [Con] -> DecsQ
deriveWalletErrorToJSON name typeVars cons = pure <$> instanceD
    (pure $ mkAppConstraintsFromVars ''ToJSON typeVars)
    (pure $ mkAppConstraint ''ToJSON $ mkTypeFromNameAndVars name typeVars)
    [funD 'toJSON $ map mkToJsonClause cons]

-- | Make body of `toJSON` function for given constructor.
mkToJsonClause :: Con -> ClauseQ
mkToJsonClause = uncurry mkToJsonClauseGeneric <=< getErrNameAndVars

-- | Make body of `toJSON` function. Accept constructor name and
-- constructor variable names.
mkToJsonClauseGeneric :: Name -> [String] -> ClauseQ
mkToJsonClauseGeneric constrName varNames = do
    bindings <- mkBindings varNames

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

--
-- FromJSON instance derivation
--

deriveWalletErrorFromJSON :: Name -> [TyVarBndr] -> [Con] -> DecsQ
deriveWalletErrorFromJSON name typeVars cons = pure <$> instanceD
    (pure $ mkAppConstraintsFromVars ''FromJSON typeVars)
    (pure $ mkAppConstraint ''FromJSON $ mkTypeFromNameAndVars name typeVars)
    [funD 'parseJSON [clause [] (mkFromJsonBody name cons) []]]

mkFromJsonBody :: Name -> [Con] -> BodyQ
mkFromJsonBody typeName cons =
    normalB [e| withObject $(stringE $ nameBase typeName) $ \o -> do
                    message <- (o .: "message" :: Parser Text)
                    (o .: "diagnostic") >>=
                        withObject "diagnostic" (\v -> $(caseE (dyn "message") msgMatches))
                    |]
  where
    msgMatches = map mkMatch cons ++ [failMatch]
    mkMatch con = do
        (name, vars) <- getErrNameAndVars con
        bindings <- mkBindings vars

        let v = dyn "v"
            constrExpr = pure $ foldl AppE (ConE name) $ map (VarE . snd) bindings
            finalStmt = noBindS [e| pure $(constrExpr) |]
            varBindStmt (name', bind) = bindS (varP bind)
                [e| $(v) .: $(stringE $ mkJsonKey name') |]

        match
            (litP $ stringL $ nameBase name)
            (normalB $ doE $ map varBindStmt bindings ++ [finalStmt])
            []
    failMatch = match wildP (normalB [e| fail "unknown WalletError tag" |]) []
