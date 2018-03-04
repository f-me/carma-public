module App.Store.DiagTree.Editor.Handlers.SharedUtils.Slide
     ( getSlide
     ) where

import Prelude hiding (id)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Maybe.Trans (MaybeT)

import Data.JSDate (LOCALE, parse, toDateTime)
import Data.Map (Map)
import Data.Map as Map
import Data.Foldable (foldM)

import Utils (toMaybeT)

import App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendSlide
     ( BackendSlide
     )

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideId
     , DiagTreeSlide (DiagTreeSlide)
     )


-- Building a slide from backend data
getSlide
  :: forall getSlideEff
   . Map DiagTreeSlideId BackendSlide
  -> DiagTreeSlideId
  -> MaybeT (Eff (locale :: LOCALE | getSlideEff)) DiagTreeSlide

getSlide flatSlides slideId = do
  { id, isRoot, ctime, header, body, resources, answers, actions } <-
    toMaybeT $ slideId `Map.lookup` flatSlides

  parsedCtime <- do
    jsDate <- liftEff $ parse ctime
    toMaybeT $ toDateTime jsDate

  slideAnswers <- foldM answerReducer Map.empty answers

  toMaybeT $ pure $
    DiagTreeSlide
      { id
      , isRoot
      , ctime: parsedCtime
      , header
      , body
      , resources
      , answers: slideAnswers
      , actions
      }

  where
    answerReducer acc { nextSlide, header, text, file } =
      getSlide flatSlides nextSlide <#> \slide ->
        let x = { nextSlide: slide, header, text, file }
         in Map.insert nextSlide x acc
