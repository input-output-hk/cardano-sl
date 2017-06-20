{-# LANGUAGE ScopedTypeVariables #-}

{-
TH helpers for Bi.

Suppose you have the following datatype:

data User
    = Login {
      login :: String
    , age   :: Int
    }
    | FullName {
      firstName  :: String
    , lastName   :: String
    , sex        :: Bool
    }

then the next deriveSimpleBi:

deriveSimpleBi ''User [
    Cons 'Login [
        Field [| login :: String |],
        Field [| age   :: Int    |],
    ],
    Cons 'FullName [
        Field [| firstName :: String |],
        Field [| age       :: Int    |],
        Unused 'secondName
    ]]

will generate:

instance Bi User where
    size = case (size :: Size String, size :: Size Int,
                 size :: Size String, size :: Size Int) of
        (ConstSize size_Login_login, ConstSize size_Login_age
         ConstSize size_FullName_firstName, ConstSize size_FullName_age)
            | size_Login == size_FullName ->
            ConstSize (1 + size_1)
          where
            size_Login = size_Login_login + size_Login_age
            size_FullName = size_FullName_firstName + size_FullName_age
        _ -> VarSize $ \x -> case x of
               val@Login{} -> getSize (login val) + getSize (age val)
               val@FullName{} -> getSize (firstName val) + getSize (age val)
    put = \x -> case x of
        val@Login{} -> labelP "User" $ do
            put (0 :: Word8)
            put (login val)
            put (age val)
        val@FullName{} -> labelP "User" $ do
            put (1 :: Word8)
            put (firstName val)
            put (age val)
    get = label "User" $ do
        tag <- get @Word8
        case tag of
            0 -> do
                login <- get
                age <- get
                pure $ Login {..}
            1 -> do
                firstName <- get
                age <- get
                let secondName = def
                pure $ FullName {..}
            _ -> Store.peekException ("Found invalid tag while getting User")
-}

module Pos.Binary.Class.TH
       ( deriveSimpleBi
       , Cons (..)
       , Field (Field)
       ) where

import           Universum
import           Unsafe                   (unsafeHead)

import           Control.Lens             (imap)
import           Data.Default             (def)
import           Data.List                (notElem, nubBy, partition)
import qualified Data.Store.Internal      as Store
import qualified Data.Text                as T
import           Formatting               (sformat, shown, (%))
import           Language.Haskell.TH
import           TH.ReifySimple           (DataCon (..), DataType (..), reifyDataType)
import           TH.Utilities             (plainInstanceD)

import qualified Pos.Binary.Class.Core    as Bi

data Cons = Cons
    { -- | Name of a constructor.
      cName   :: Name
      -- | Field of a constructor.
    , cFields :: [Field]
    }

data Field
    = Field {
    -- ^ The constructor means that you want
    -- a field to participate in serialisation/deserialization
      fFieldAndType :: ExpQ
    -- ^ You're expected to write something like @[|foo :: Bar|]@ here
    }
    | Unused {
    -- ^ The constructor means that you don't want
    -- a field to participate in serialisation/deserialization
      fName :: Name
    -- ^ Name of unused field
    }

-- | Turn something like @[|foo :: Bar|]@ into @(foo, Bar)@.
expToNameAndType :: ExpQ -> Q (Name, Type)
expToNameAndType ex = ex >>= \case
    SigE (VarE n) t -> pure (n, t)
    other           -> fail $ "expToNameAndType: the expression should look \
                              \like [|fname :: FType|], but it doesn't: "
                              <> show other

fieldToPair :: Field -> Q (Name, Maybe Type)
fieldToPair (Unused nm) = pure (nm, Nothing)
fieldToPair (Field ex)  = over _2 Just <$> expToNameAndType ex

-- Some part of code copied from
-- https://hackage.haskell.org/package/store-0.4.3.1/docs/src/Data-Store-TH-Internal.html#makeStore

-- | Takes the name of datatype and constructors of datatype and generates Bi instances.
-- You should pass all constructors explicitly. Also, you should pass all fields explicitly,
-- each of them should be @Field@ or @Unused@,
-- and the real type of field and the passed (in the Field) type should be same.
-- All field of datatype should be named explicitly.
-- The numbers of constructors must be at least one and at most 255.
-- The order of fields matter: it corresponds to order of put's and get's.
-- If some of these statements is violated,
-- you will get compile error with the corresponding message.
deriveSimpleBi :: Name -> [Cons] -> Q [Dec]
deriveSimpleBi headTy constrs = do
    when (null constrs) $
        failText "You passed no constructors to deriveSimpleBi"
    when (length constrs > 255) $
        failText "You passed too many constructors to deriveSimpleBi"
    when (length (nubBy ((==) `on` cName) constrs) /= length constrs) $
        failText "You passed two constructors with the same name"
    dt <- reifyDataType headTy
    case matchAllConstrs constrs (dtCons dt) of
        MissedCons cons ->
            failText .
                sformat ("Constructor '"%shown%"' isn't passed to deriveSimpleBi") $
                cons
        UnknownCons cons ->
            failText .
                sformat ("Unknown constructor '"%shown%"' is passed to deriveSimpleBi") $
                cons
        MatchedCons matchedConstrs ->
            forM_ (zip constrs matchedConstrs) $ \(Cons{..}, DataCon{..}) -> do
                let realFields = mapMaybe (\(n, t) -> (,t) <$> n) dcFields
                when (length realFields /= length dcFields) $
                    failText $ sformat ("Some field of "%shown
                                       %" constructor doesn't have an explicit name") cName
                cResolvedFields <- mapM fieldToPair cFields
                case checkAllFields cResolvedFields realFields of
                    MissedField field ->
                        failText $ sformat ("Field '"%shown%"' of the constructor '"
                                            %shown%"' isn't passed to deriveSimpleBi")
                                   field cName
                    UnknownField field ->
                        failText $ sformat ("Unknown field '"%shown%"' of the constructor '"
                                            %shown%"' is passed to deriveSimpleBi")
                                   field cName
                    TypeMismatched field realType passedType ->
                        failText $ sformat ("The type of '"%shown%"' of the constructor '"
                                            %shown%"' is mismatched: real type '"
                                            %shown%"', passed type '"%shown%"'")
                                   field cName realType passedType
                    MatchedFields -> pass
    ty <- conT headTy
    makeBiInstanceTH ty <$> biSizeExpr <*> biPutExpr <*> biGetExpr
  where
    shortNameTy :: Text
    shortNameTy = toText $ nameBase headTy
    -- Meta information about constructors --

    -- Constructor and its used fields.
    filteredConstrs :: [Cons]
    filteredConstrs = map (\Cons{..} -> Cons cName (filter isUsed cFields)) constrs

    -- All used fields:
    -- [Fields of Constr1 [(FieldName, FieldType)],  Fields of Constr2 [(FieldName, FieldType)], ..]
    -- @filteredConstrs@ and @allUsedFields@ are almost same,
    -- but @filteredConstrs@ store the name of constructors.
    allUsedFields :: [[Q (Name, Type)]]
    allUsedFields = map (\Cons{..} ->
                        map (\Field{..} -> expToNameAndType fFieldAndType) cFields)
                    filteredConstrs

    -- Useful variables for @size@, @put@, @get@ --
    tagType :: TypeQ
    tagType = [t| Word8 |]

    mbTagSize :: ExpQ
    mbTagSize = if length constrs >= 2 then [| 1 |] else [| 0 |]

    -- Helpers --
    failText :: MonadFail m => T.Text -> m a
    failText = fail . toString

    isUsed :: Field -> Bool
    isUsed (Unused _) = False
    isUsed _          = True

    -- Put definition --
    biPutExpr :: Q Exp
    biPutExpr = do
        x <- newName "x"
        lam1E (varP x) $
          caseE (varE x) $
              imap biPutConstr filteredConstrs

    -- Generate the following code:
    -- val@Constr{} -> label "TypeName" $ do
    --   put (3 :: Word8)
    --   put (field1 val)
    --   put (field2 val)
    --   put (field3 val)
    biPutConstr :: Int -> Cons -> MatchQ
    biPutConstr ix (Cons cName cFields) = do
        val <- newName $ if length cFields > 0 then "val" else "_"
        match (asP val (recP cName [])) (body (varE val)) []
      where
        body val = normalB $ appE [| Bi.labelP shortNameTy |] $
            if length constrs >= 2 then
                doE (putTag ix : map (putField val) cFields)
            else
                doE (map (putField val) cFields)

    putTag :: Int -> Q Stmt
    putTag ix = noBindS [| Bi.put (ix :: $tagType) |]

    putField :: ExpQ -> Field -> Q Stmt
    putField val Field{..} = do
        (fName, _) <- expToNameAndType fFieldAndType
        noBindS [| Bi.put ($(varE fName) $val) |]
    putField _  (Unused _) = fail "Something went wrong: put Unused field"

    -- Size definition --
    biSizeExpr :: Q Exp
    biSizeExpr = do
        let sizes :: [ExpQ]
            sizes = map (sizeAtType . fmap snd) $ concat allUsedFields
        caseE (tupE sizes) $
            -- when there are no fields, the second branch will be
            -- redundant so we don't generate it at all
            [matchConstSize] ++
            [matchVarSize | not (all null allUsedFields)]

    -- Generate code like "size :: Word8", "size :: Int"
    sizeAtType :: TypeQ -> ExpQ
    sizeAtType ty = [| Bi.size :: Store.Size $ty |]

    -- Generate a unique name per each constructor:
    genConsUniques :: String -> Q [Name]
    genConsUniques pref =
        forM filteredConstrs $ \Cons{..} ->
            -- TODO: this won't work if the constructor name is an
            -- operator. We need to mangle those names somehow.
            newName (pref ++ "_" ++ nameBase cName)

    -- Generate a unique name per each used field:
    --   pref_Foo_fa, pref_Foo_fb, ...
    --   pref_Bar_fx, pref_Bar_fy, ...
    genFieldUniques :: String -> Q [[Name]]
    genFieldUniques pref =
        forM filteredConstrs $ \Cons{..} ->
        forM cFields $ \Field{..} -> do
            (fName, _) <- expToNameAndType fFieldAndType
            -- TODO: this won't work if the constructor name is an
            -- operator. We need to mangle those names somehow.
            newName (pref ++ "_" ++ nameBase cName
                          ++ "_" ++ nameBase fName)

    -- Generate the following code:
    -- ( ConstSize size_Foo_fa
    -- , ConstSize size_Bar_fx, ConstSize size_Bar_fy, ConstSize size_Bar_fz
    -- , ConstSize size_Baz_fn, ConstSize size_Baz_fm )
    --   | size_Foo == size_Bar && size_Bar == size_Baz ->
    --     ConstSize (tagSize + size_Foo)
    --   where
    --     size_Foo = size_Foo_fa
    --     size_Bar = size_Bar_fx + size_Bar_fy + size_Bar_fz
    --     size_Baz = size_Baz_fn + size_Baz_fm
    matchConstSize :: MatchQ
    matchConstSize
      | null allUsedFields     =    -- no constructors = size 0
          match wildP (normalB [| Store.ConstSize 0 |]) []
      | all null allUsedFields =    -- no fields anywhere = size of the tag
          match wildP (normalB [| Store.ConstSize $mbTagSize |]) []
      | otherwise = do
          -- Names and vars for total sizes:
          --     size_Foo, size_Bar, size_Baz, ...
          -- Names and vars for field sizes:
          --     size_Foo_fa, ...
          --     size_Bar_fx, size_Bar_fy, ...                 ...
          -- Calculations of total sizes:
          --     where size_Foo = size_Foo_fa
          --           size_Bar = size_Bar_fx + size_Bar_fy    ...
          totalNames <- genConsUniques "size"
          let totals     = map varE totalNames
              firstTotal = unsafeHead totals
          fieldSizeNames <- genFieldUniques "size"
          let fieldSizes = (map . map) varE fieldSizeNames
          let mkTotalDecl ts xs = valD (varP ts) (normalB (sumE xs)) []
              totalsDecls       = zipWith mkTotalDecl totalNames fieldSizes
          -- The giant tuple pattern with all sizes:
          --     (ConstSize size_Foo_fa, ConstSize size_Bar_fx, ...)
          -- The size equality guard:
          --     | size_Foo == size_Bar && size_Bar == size_Baz && ...
          -- The result:
          --     -> ConstSize (size_Foo + tagSize)
          let casePattern  = tupP [ [p| Store.ConstSize $(varP fs) |]
                                  | fs <- concat fieldSizeNames]
              caseGuard    = andE $ zipConsecutive
                                      (\a b -> infixApp a [|(==)|] b)
                                      totals
              caseResult   = [| Store.ConstSize ($firstTotal + $mbTagSize) |]
          -- Put it all together!
          match casePattern
                (guardedB [normalGE caseGuard caseResult])
                totalsDecls

    -- Generate the following code:
    -- _ -> VarSize $ \x -> tagSize +
    --    case x of
    --        val@Bar{} -> getSize (f1 val) + getSize (f2 val)
    --        val@Baz{} -> getSize (f1 val)
    matchVarSize :: MatchQ
    matchVarSize = do
        x <- newName "x"
        let branches = map matchVarCons filteredConstrs
        match wildP
              (normalB
                 [| Store.VarSize $ \ $(varP x) ->
                        $mbTagSize + $(caseE (varE x) branches) |])
              []

    -- Generate the following code:
    --     val@Bar{} -> getSize (field1 val) + getSize (field2 val)
    matchVarCons :: Cons -> MatchQ
    matchVarCons Cons{..} = do
        -- we assume that the constructor is filtered and has only Field
        fieldNames <- mapM (fmap fst . fieldToPair) cFields
        val <- newName $ if length fieldNames > 0 then "val" else "_"
        match (asP val (recP cName [])) (body (varE val) fieldNames) []
      where
        body val fieldNames = normalB $
            sumE [[| Bi.getSize ($(varE f) $val) |] | f <- fieldNames]

    -- Get definition --
    biGetExpr :: Q Exp
    biGetExpr =  appE [| Bi.label shortNameTy |] $ case constrs of
        []        ->
            failText $ sformat ("Attempting to peek type without constructors "%shown) headTy
        (cons:[]) ->
            (biGetConstr cons) -- There is one constructor
        _         ->do
            let tagName = mkName "tag"
            let getMatch (ix, con) = match (litP (IntegerL ix)) (normalB (biGetConstr con)) []
            let mismatchConstr =
                    match wildP (normalB
                        [| Store.peekException ("Found invalid tag while getting " <> shortNameTy) |]) []
            doE
                [ bindS (varP tagName) [| Bi.get |]
                , noBindS (caseE
                                (sigE (varE tagName) tagType)
                                (map getMatch (zip [0..] constrs) ++ [mismatchConstr]))
                ]

    biGetConstr :: Cons -> Q Exp
    biGetConstr (Cons name []) = appE (varE 'pure) (conE name)
    biGetConstr Cons{..} = do
        let (usedFields, unusedFields) = partition isUsed cFields

        fieldNames :: [Name] <- mapM (fmap fst . fieldToPair) usedFields
        varNames   :: [Name] <- mapM (newName . nameBase) fieldNames
        varPs :: [Pat] <- mapM varP varNames
        biGets :: [Exp] <- replicateM (length varPs) [| Bi.get |]
        bindExprs :: [Stmt] <- mapM (uncurry bindS . bimap pure pure) (zip varPs biGets)
        let recWildUsedVars = map (\(f, ex) -> (f,) <$> varE ex) $ zip fieldNames varNames
        let recWildUnusedVars = map (\f -> (fName f,) <$> [| def |]) unusedFields
        recordWildCardReturn <- noBindS $
                                appE (varE 'pure) $
                                recConE cName $
                                recWildUsedVars ++ recWildUnusedVars
        doE $ map pure bindExprs ++ [pure recordWildCardReturn]

makeBiInstanceTH :: Type -> Exp -> Exp -> Exp -> [Dec]
makeBiInstanceTH ty sizeE putE getE = one $
  plainInstanceD
        [] -- empty context
        (AppT (ConT ''Bi.Bi) ty)
        [ ValD (VarP 'Bi.size) (NormalB sizeE) []
        , ValD (VarP 'Bi.put)  (NormalB putE) []
        , ValD (VarP 'Bi.get)  (NormalB getE) []
        ]

data MatchConstructors
    = MatchedCons [DataCon]
    -- ^ Constructors in matched order
    | MissedCons Name
    -- ^ Some constructor aren't passed
    | UnknownCons Name
    -- ^ Passed unknown constructor

matchAllConstrs :: [Cons] -> [DataCon] -> MatchConstructors
matchAllConstrs (map cName -> passedNames) realCons@(map dcName -> realNames)
    | Just nm <- passedNames `inclusion` realNames = UnknownCons nm
    | Just nm <- realNames `inclusion` passedNames = MissedCons nm
    | otherwise =
        let ret = catMaybes $ map (\x -> find ((x==) . dcName) realCons) passedNames in
        if length ret /= length passedNames then
            error "Something went wrong. Matched list of constructors has different length"
        else
            MatchedCons ret
  where
    inclusion :: [Name] -> [Name] -> Maybe Name
    inclusion c1 c2 = find (`notElem` c2) c1

data MatchFields
    = MatchedFields
    -- ^ All fields are matched
    | MissedField Name
    -- ^ Some field aren't passed
    | UnknownField Name
    -- ^ Passed field with unknown name
    | TypeMismatched Name Type Type
    -- ^ Some field has mismatched type

checkAllFields :: [(Name, Maybe Type)] -> [(Name, Type)] -> MatchFields
checkAllFields passedFields realFields
    | Just nm <- map fst passedFields `inclusion` map fst realFields = UnknownField nm
    | Just nm <- map fst realFields `inclusion` map fst passedFields = MissedField nm
    | otherwise =
        let ret = catMaybes $ map (\x -> find ((fst x ==) . fst) realFields) passedFields in
        if length ret /= length passedFields then
            error "Something went wrong. Matched list of fields has different length"
        else
            case dropWhile checkTypes (zip realFields passedFields) of
                []                                -> MatchedFields
                (((n, real), (_, Just passed)):_) -> TypeMismatched n real passed
                (((_, _), (_, Nothing)):_)        -> error "Something went wrong: illegal mismatch type"
  where
    checkTypes :: ((Name, Type), (Name, Maybe Type)) -> Bool
    checkTypes (_, (_, Nothing))       = True
    checkTypes ((_, t1), (_, Just t2)) = t1 == t2

    inclusion :: [Name] -> [Name] -> Maybe Name
    inclusion c1 c2 = find (`notElem` c2) c1


----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

foldr1E :: ExpQ -> [ExpQ] -> ExpQ
foldr1E f = foldr1 (\a b -> infixApp a f b)

foldl1E :: ExpQ -> [ExpQ] -> ExpQ
foldl1E f = foldl1 (\a b -> infixApp a f b)

-- | Put '(+)' between expressions.
sumE :: [ExpQ] -> ExpQ
sumE [] = [| 0 |]
sumE xs = foldl1E [|(+)|] xs

-- | Put '(&&)' between expressions.
andE :: [ExpQ] -> ExpQ
andE [] = [| True |]
andE xs = foldr1E [|(&&)|] xs

-- | Zip consecutive elements (a+b, b+c, c+d, ...)
zipConsecutive :: (a -> a -> b) -> [a] -> [b]
zipConsecutive f xs = zipWith f xs (drop 1 xs)
