module Router
     ( Location (..)
     , initRouter
     , navigateToRoute
     ) where

import Prelude

import Data.Generic (class Generic, gShow)
import Data.Maybe (fromMaybe)

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)

import DOM (DOM)

import Routing.Match (Match)
import Routing.Match.Class (lit, str)
import Routing.Hash (matches, setHash)


data Location
  = Empty
  | DiagTreeEdit
  | NotFound

derive instance eqLocation :: Eq Location
derive instance genericShow :: Generic Location
instance eqShow :: Show Location where show = gShow

getLocationRoute :: Location -> String
getLocationRoute Empty = ""
getLocationRoute NotFound = "-"
getLocationRoute DiagTreeEdit = "diag-tree-edit"


routing :: Match Location
routing  =  f DiagTreeEdit (lit "diag-tree-edit")
        <|> (NotFound <$ str)
        <|> pure Empty

  where

    -- `lit "foo"` matches both "foo" and "foo/smth"
    -- but wee need to match only "foo", so, this helps to solve it.
    f :: Location -> Match Unit -> Match Location
    f l m = (NotFound <$ (m *> str)) <|> (l <$ m)


initRouter
  :: forall eff
  .  (Location -> Eff (dom :: DOM, ref :: REF | eff) Unit)
  -> Eff (dom :: DOM, ref :: REF | eff) Unit
initRouter notify = void $ matches routing $ \oldRoute newRoute ->

  let isPassed = fromMaybe true $ (_ /= newRoute) <$> oldRoute
      defaultRoute = getLocationRoute DiagTreeEdit

      navigate =
        if newRoute == Empty
           then setHash $ defaultRoute
           else notify newRoute

   in when isPassed navigate


navigateToRoute :: forall eff. Location -> Eff (dom :: DOM | eff) Unit
navigateToRoute Empty    = pure unit
navigateToRoute NotFound = pure unit
navigateToRoute route    = setHash $ getLocationRoute route
