{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.ExecMode.Context
    ( (:::)
    , HasLens(..)
    , modeContext
    ) where

import           Universum

import           Language.Haskell.TH

import           Pos.Util.Util       (HasLens (..), lensOf')

-- | Bundle a tag and a value. The 'modeContext' function will transform
-- @Tag ::: Integer@ to @Integer@ and use @Tag@ as meta-info about the field.
data tag ::: value

{- | Generate a mode context record with 'HasLens' instances.

The 'modeContext' function takes a declaration like this:

@
    data PatakCtx ssc = PatakCtx
        !(Buba    ::: Buba)
        !(HuhaTag ::: Huha ssc)
        !Bardaq
@

and turns it into declarations like this:

@
    data PatakCtx ssc = PatakCtx
        !Buba
        !(Huha ssc)
        !Bardaq

    instance HasLens Buba (PatakCtx ssc) Buba where
        lensOf = \f (PatakCtx b h q) -> fmap (\b' -> PatakCtx b' h q) (f b)

    instance HasLens HuhaTag (PatakCtx ssc) (Huha ssc) where
        lensOf = \f (PatakCtx b h q) -> fmap (\h' -> PatakCtx b h' q) (f h)

    instance {-# OVERLAPPABLE #-}
        HasLens tag Bardaq         r =>
        HasLens tag (PatakCtx ssc) r
      where
        lensOf =
            ( \f (PatakCtx b h q) -> fmap (\q' -> PatakCtx b h q') (f q)
            ) . lensOf @tag

Notice that types annotated with '(:::)' get turned into fields. The rest
of the fields are inherited from the parent (a field without a tag annotation)
using an overlappable instance. Only one parent is allowed.
@
-}

modeContext :: DecsQ -> DecsQ
modeContext dsQ = do
    ds <- dsQ
    (dCxt, dName, dTyVarBndrs, dKs, dCons, dCxt') <- case ds of
        [DataD dCxt dName dTyVarBndrs dKs dCons dCxt'] ->
            return (dCxt, dName, dTyVarBndrs, dKs, dCons, dCxt')
        _ ->
            fail "modeContext: Expected a single data declaration"
    let
        tyParam (PlainTV tvName)         = VarT tvName
        tyParam (KindedTV tvName tvKind) = VarT tvName `SigT` tvKind
        ty = foldl' AppT (ConT dName) (map tyParam dTyVarBndrs)
    (conName, conTys, mConTyNames) <- case dCons of
        [NormalC conName conTys] -> return $
            (conName, conTys, Nothing)
        [RecC conName conNamedTys] -> return $
            let (conTys, conTyNames) = unzip [ ((b, t), n) | (n, b, t) <- conNamedTys ]
            in (conName, conTys, Just conTyNames)
        _ ->
            fail "modeContext: Expected a single normal constructor"
    let
      eLens :: Int -> ExpQ
      eLens i = do
          -- (\f (Buba x0 x1 patak x3) -> fmap (\patak' -> Buba x0 x1 patak' x3) (f patak))
          xNames1 <- traverse (\j -> newName ("x" ++ show j)) [0..i-1] -- names before patak
          xNames2 <- traverse (\j -> newName ("x" ++ show j)) [i+1..length conTys-1] -- names after patak
          patakName <- newName "patak"
          patakPrimeName <- newName "patak'"
          let
            bubaPatQ = return $
                ConP conName (map VarP $ xNames1 ++ [patakName] ++ xNames2)
            bubaExpQ = return $
                foldl' AppE (ConE conName)
                    (map VarE $ xNames1 ++ [patakPrimeName] ++ xNames2)
            patakExpQ = return $ VarE patakName
            patakPrimePatQ = return $ VarP patakPrimeName
            fmapQ a b = varE 'fmap `appE` a `appE` b
          fName <- newName "f"
          lamE [varP fName, bubaPatQ] $
              fmapQ (lamE [patakPrimePatQ] bubaExpQ)
                  (varE fName `appE` patakExpQ)
    (unzip -> (conNewTys, concat -> hasLensInstances)) <-
        forM (zip [0..] conTys) $ \(i, (tyBang, conTy)) -> do
            case conTy of
                ConT t `AppT` tagTy `AppT` valTy | t == ''(:::) -> do
                    hasLensInst <-
                        [d|instance patak ~ $(pure valTy) => HasLens $(pure tagTy) $(pure ty) patak where
                               lensOf = $(eLens i)|]
                    return ((tyBang, valTy), hasLensInst)
                _ -> do
                    hasLensInst <-
                        [d|instance {-# OVERLAPPABLE #-} HasLens tag $(pure conTy) patak => HasLens tag $(pure ty) patak where
                               lensOf = $(eLens i) . lensOf' (Proxy :: Proxy tag)|]
                    return ((tyBang, conTy), hasLensInst)
    let dNewCons = case mConTyNames of
            Nothing -> [NormalC conName conNewTys]
            Just conTyNames ->
                let conNamedTys = [(n, b, t) | ((b, t), n) <- zip conNewTys conTyNames]
                in [RecC conName conNamedTys]
        newDataDecl = DataD dCxt dName dTyVarBndrs dKs dNewCons dCxt'
    return (newDataDecl : hasLensInstances)
