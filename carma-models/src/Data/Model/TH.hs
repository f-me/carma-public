{-# LANGUAGE TemplateHaskell #-}

{-| Template Haskell helpers for carma-models. -}

module Data.Model.TH
    ( mkIdents
    , typeRepToType
    )

where

import Control.Monad

import qualified Data.HashMap.Strict as HM
import           Data.Typeable

import Language.Haskell.TH

import Data.Model.Types


-- | Generate a list of Ident declarations and bind `idents` to a
-- hashmap mapping String labels to produced declarations.
--
-- Example:
--
-- > mkIdents 'Role [("all", 5)]
--
-- compiles to
--
-- > all = Ident 5 :: IdentI Role
-- > idents = HM.fromList [("all", all)]
mkIdents :: (Q Type)
         -> [(String, Integer)]
         -> Q [Dec]
mkIdents modelTy names = do
    let list = mkName "idents"
        defTy = [t|IdentI $(modelTy)|]
    identDefs <- forM names $
      \(nm, i) -> do
        let nm' = mkName nm
        -- Type sig
        s <- sigD nm' defTy
        -- Value def
        d <- valD (varP nm')
             (normalB [e|Ident $(litE $ integerL i)|]) []
        return [s, d]
    idMapTy  <- sigD list [t|HM.HashMap String $defTy|]
    let pairs = map (\(nm, _) -> tupE [litE $ stringL nm, varE $ mkName nm])
                names
    idMapDef <- valD (varP list) (normalB ((varE 'HM.fromList) `appE` (listE pairs))) []
    return $ concat $ identDefs ++ [[idMapTy, idMapDef]]


-- | Convert 'TypeRep' to Template Haskell 'Type'.
typeRepToType :: TypeRep -> Type
typeRepToType tr =
    let
        tyConToType tyCon = ConT $ mkName $ tyConName tyCon
        (conTr, trs)      = splitTyConApp tr
    in
      foldl (\t m -> AppT t (typeRepToType m)) (tyConToType conTr) trs
