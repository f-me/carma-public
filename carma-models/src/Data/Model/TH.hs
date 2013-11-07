{-# LANGUAGE TemplateHaskell #-}

module Data.Model.TH
    ( mkIdents
    )

where

import Control.Monad

import qualified Data.HashMap.Strict as HM

import Language.Haskell.TH

import Data.Model.Types


-- | Generate a list of Ident declarations and bind `idents` to a
-- hashmap mapping String labels to produced declarations.
--
-- > mkIdents 'Role [("all", 5)]
-- all = Ident 5 :: IdentI Role
-- idents = HM.fromList [("all", all)]
mkIdents :: (Q Type)
         -> [(String, Integer)]
         -> Q [Dec]
mkIdents modelTy names = do
    Just identTy <- lookupTypeName "IdentI"
    let list = mkName "idents"
        defTy = (conT identTy) `appT` modelTy
    identDefs <- forM names $
      \(nm, i) -> do
        let nm' = mkName nm
        -- Type sig
        s <- sigD nm' defTy
        -- Value def
        d <- valD (varP nm')
             (normalB ((conE 'Ident) `appE` (litE $ integerL i))) []
        return [s, d]
    idMapTy  <- sigD list [t|HM.HashMap String $defTy|]
    let pairs = map (\(nm, _) -> tupE [litE $ stringL nm, varE $ mkName nm])
                names
    idMapDef <- valD (varP list) (normalB ((varE 'HM.fromList) `appE` (listE pairs))) []
    return $ concat $ identDefs ++ [[idMapTy, idMapDef]]
