{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | TH helpers for Bi
module Pos.Binary.Class.TH
       ( deriveSimpleBi
       , Cons (..)
       , Field (..)
       ) where

import           Universum

import           Data.Default          (def)
import qualified Data.Text             as T
import           Formatting            (sformat, shown, (%))
import           Language.Haskell.TH
import           TH.ReifySimple        (DataCon (..), DataType (..), reifyDataType)
import           TH.Utilities          (plainInstanceD)

import           Pos.Binary.Class.Core (Bi (..))
import qualified Pos.Binary.Class.Core as Bi

data Cons
    = Cons {
      cName   :: Name
    , cFields :: [Field]
    }

data Field
    = Field {
      fName :: Name
    , fType :: Name
    }
    -- ^ Field name and field type
    | Unused Name
    -- ^ Name of unused field
-- Some part of implementation copied from
-- https://hackage.haskell.org/package/store-0.4.3.1/docs/src/Data-Store-TH-Internal.html#makeStore
deriveSimpleBi :: Name -> [Cons] -> Q [Dec]
deriveSimpleBi headTy constrs = do
    dt <- reifyDataType headTy
    case (matchAllConstrs constrs (dtCons dt)) of
        MissedCons missedCons ->
            failText .
                sformat ("Constructor '"%shown%"' isn't passed to deriveSimpleBi") $
                missedCons
        UnknownCons unknownCons ->
            failText .
                sformat ("Unknown constructor '"%shown%"' is passed to deriveSimpleBi") $
                unknownCons
        MatchedCons matchedConstrs ->
            forM_ (zip constrs matchedConstrs) $ \(Cons{..}, DataCon{..}) -> do
                whenJust (checkAllFields cFields dcFields) $ \field ->
                    failText $ sformat ("Unknown field '"%shown%"' of constructor '"
                                        %shown%"' is passed to deriveSimpleBi")
                                  field cName
    fail "not implemented yet"
  where
    failText = fail . T.unpack
    isUsed :: Field -> Bool
    isUsed (Unused _) = False
    isUsed _          = True

    tagType :: Name
    tagType = ''Word8

    -- Expression used for the definition of get.
    biGetExpr :: Q Exp
    biGetExpr = case constrs of
        []        -> failText $ sformat ("Attempting to peek type without constructors "%shown) headTy
        (cons:[]) -> biGetConstr cons -- There is one consturctors
        _         -> do
            let tagName = mkName "tag"
            let getMatch (ix, con) = match (litP (IntegerL ix)) (normalB (biGetConstr con)) []
            let mismatchConstr =
                    match wildP (normalB
                        [| peekException (sformat ("Found invalid tag while getting "%build) headTy) |]) []
            doE
                [ bindS (varP tagName) [| Bi.get |]
                , noBindS (caseE
                                (sigE (varE tagName) (conT tagType))
                                (map getMatch (zip [0..] constrs) ++ [mismatchConstr]))
                ]

    biGetConstr :: Cons -> Q Exp
    biGetConstr (Cons name []) = appE (varE 'pure) (conE name)
    biGetConstr Cons{..} = do
        let usedFields = filter isUsed cFields
        let unusedFields = filter (not . isUsed) cFields

        varNames :: [Name] <- mapM (newName . show . fName) usedFields
        varPs :: [Pat] <- mapM varP varNames
        biGets :: [Exp] <- replicateM (length varPs) [| Bi.get |]
        bindExprs :: [Stmt] <- mapM (uncurry bindS . bimap pure pure) (zip varPs biGets)
        let recWildUsedVars = map (\(f, ex) -> (fName f,) <$> varE ex) $ zip usedFields varNames
        let recWildUnusedVars = map (\f -> (fName f,) <$> [| def |]) unusedFields
        recordWildCardReturn <- noBindS $
                                appE (varE 'pure) $
                                recConE cName $
                                recWildUsedVars ++ recWildUnusedVars
        doE $ map pure bindExprs ++ [pure recordWildCardReturn]

makeBiInstanceTH :: Type -> Exp -> Exp -> Exp -> Dec
makeBiInstanceTH ty sizeE putE getE =
  plainInstanceD
        [] -- empty context
        (AppT (ConT ''Bi) ty)
        [ ValD (VarP 'size) (NormalB sizeE) []
        , ValD (VarP 'put) (NormalB putE) []
        , ValD (VarP 'get) (NormalB getE) []
        ]

data ConsFail
    = MatchedCons [DataCon]
    -- ^ Constructors in matched order
    | MissedCons Name
    | UnknownCons Name

matchAllConstrs :: [Cons] -> [DataCon] -> ConsFail
matchAllConstrs = undefined

checkAllFields :: [Field] -> [(Maybe Name, Type)] -> Maybe Name
checkAllFields = undefined

