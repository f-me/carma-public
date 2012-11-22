

{-# LANGUAGE OverloadedStrings #-}
-- NB. This is just a temporary solution for rendering SMS templates
module DumbTemplate where


import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map



render :: Map Text Text -> Map Text (Map Text Text) -> Text -> Text
render varMap dicMap = T.concat . loop
  where
    loop tpl = case T.breakOn "$" tpl of
      (txt, "") -> [txt]
      (txt, tpl') -> case T.breakOn "$" $ T.tail tpl' of
        (exp, "")    -> [txt, evalExp exp]
        (exp, tpl'') -> txt : evalExp exp : loop (T.tail tpl'')

    evalExp e = case T.breakOn "#" e of
      (var, "")     -> evalVar var
      (dic, keyVar) -> evalDic dic (evalVar keyVar)

    evalVar v = Map.findWithDefault v v varMap

    evalDic dicNm k
      = Map.findWithDefault (T.concat [dicNm, "#", k]) k
      $ Map.findWithDefault Map.empty dicNm dicMap
