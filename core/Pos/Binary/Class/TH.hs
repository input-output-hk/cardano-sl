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

deriveSimpleBi :: Name -> [Cons] -> Q [Dec]
deriveSimpleBi dataName constrs = do
    dt <- reifyDataType dataName
    case (matchAllConstrs constrs (dtCons dt)) of
        MissedCons missedCons ->
            fail . T.unpack .
                    sformat ("Constructor '"%shown%"' isn't passed to deriveSimpleBi") $
                    missedCons
        UnknownCons unknownCons ->
            fail . T.unpack .
                    sformat ("Unknown constructor '"%shown%"' is passed to deriveSimpleBi") $
                    unknownCons
        MatchedCons matchedConstrs ->
            forM_ (zip constrs matchedConstrs) $ \(Cons{..}, DataCon{..}) -> do
                whenJust (checkAllFields cFields dcFields) $ \field ->
                    fail . T.unpack $
                            sformat ("Unknown field '"%shown%"' of constructor '"
                                     %shown%"' is passed to deriveSimpleBi")
                            field cName
    fail "not implemented yet"
  where
    isUsed :: Field -> Bool
    isUsed (Unused _) = False
    isUsed _          = True

    biGetForConstr :: Cons -> Q Exp
    biGetForConstr (Cons name []) = appE (varE 'pure) (conE name)
    biGetForConstr Cons{..} = do
        let usedFields = filter isUsed cFields
        let unusedFields = filter (not . isUsed) cFields

        varNames :: [Name] <- mapM (newName . show . fName) usedFields
        varPs :: [Pat] <- mapM varP varNames
        biGets :: [Exp] <- replicateM (length varPs) (varE 'Bi.get)
        bindExprs :: [Stmt] <- mapM (uncurry bindS . bimap pure pure) (zip varPs biGets)
        let recWildUsedVars = map (\(f, ex) -> (fName f,) <$> varE ex) $ zip usedFields varNames
        let recWildUnusedVars = map (\f -> (fName f,) <$> varE 'def) unusedFields
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

