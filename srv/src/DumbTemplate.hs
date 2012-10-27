

{-# LANGUAGE OverloadedStrings #-}
-- NB. This is just a temporary solution for rendering SMS templates
module DumbTemplate where


import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map



render :: Map Text Text -> Text -> Text
render varMap = T.concat . loop
  where
    loop tpl = case T.breakOn "$" tpl of
      (txt, "") -> [txt]
      (txt, tpl') -> case rest of
        "" -> [txt,varVal]
        _  -> txt : varVal : loop (T.tail rest)
        where
          (var, rest) = T.breakOn "$" $ T.tail tpl'
          varVal = Map.findWithDefault var var varMap
