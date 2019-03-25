module Router
     ( Location (..)
     , initRouter
     , navigateToRoute
     ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (fromMaybe)

import Control.Alt ((<|>))
import Effect (Effect)

import Routing.Match (Match, lit, str)
import Routing.Hash (matches, setHash)


data Location
   = Empty
   | DiagTreeEditPartial
   | NotFound

derive instance eqLocation :: Eq Location
derive instance genericLocation :: Generic Location _
instance showLocation :: Show Location where show = genericShow

-- Backward mapping for navigating by `Location`s.
locationHash :: Location -> String
locationHash Empty = ""
locationHash NotFound = "-"
locationHash DiagTreeEditPartial = "partial/diag-tree-edit"


routing :: Match Location
routing = go where
  go  =  f DiagTreeEditPartial (partials *> lit "diag-tree-edit")
     <|> (NotFound <$ str)
     <|> pure Empty

  partials = lit "partial"

  -- `lit "foo"` matches both "foo" and "foo/smth"
  -- but wee need to match only "foo", so, this helps to solve it.
  f :: Location -> Match Unit -> Match Location
  f l m = (NotFound <$ (m *> str)) <|> (l <$ m)


initRouter :: (Location -> Effect Unit) -> Effect Unit
initRouter notify = void $ matches routing $ \oldRoute newRoute ->

  let isPassed = fromMaybe true $ (_ /= newRoute) <$> oldRoute
      defaultRoute = locationHash DiagTreeEditPartial

      navigate =
        if newRoute == Empty
           then setHash defaultRoute
           else notify newRoute

   in when isPassed navigate


navigateToRoute :: Location -> Effect Unit
navigateToRoute Empty    = pure unit
navigateToRoute NotFound = pure unit
navigateToRoute route    = setHash $ locationHash route
