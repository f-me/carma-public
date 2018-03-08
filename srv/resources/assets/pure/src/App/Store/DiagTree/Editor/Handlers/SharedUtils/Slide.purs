module App.Store.DiagTree.Editor.Handlers.SharedUtils.Slide
     ( getSlide
     ) where

import Prelude hiding (id)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.MonadZero (guard)
import Control.Alt ((<|>))

import Data.Maybe (Maybe (..), isJust)
import Data.JSDate (LOCALE, parse, toDateTime)
import Data.Map (Map)
import Data.Map as Map
import Data.Foldable (foldM)
import Data.Array (length, head, snoc)

import Utils (toMaybeT)
import Utils.DiagTree.Editor (diagTreeSlideActionFromBackend)

import App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendSlide
     ( BackendSlide
     )

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideId
     , DiagTreeSlide (DiagTreeSlide)
     , DiagTreeSlideResourceAttachment (..)
     )


-- Building a slide from backend data
getSlide
  :: forall getSlideEff
   . Map DiagTreeSlideId BackendSlide
  -> DiagTreeSlideId
  -> MaybeT (Eff (locale :: LOCALE | getSlideEff)) DiagTreeSlide

getSlide flatSlides slideId = do
  s@{ id, isRoot, header, body, actions } <-
    toMaybeT $ slideId `Map.lookup` flatSlides

  ctime <- do
    jsDate <- liftEff $ parse s.ctime
    toMaybeT $ toDateTime jsDate

  resources <- toMaybeT $ foldM resourceReducer [] s.resources
  answers   <- foldM answerReducer Map.empty s.answers

  -- "actions" must be an array of one element or empty array)
  action <- toMaybeT $ do
    let len = length actions
    guard $ len == 0 || len == 1

    if len == 1
       then head actions >>= diagTreeSlideActionFromBackend <#> Just
       else pure Nothing

  toMaybeT $ pure $
    DiagTreeSlide
      { id, isRoot, ctime, header, body, resources, action, answers }

  where
    resourceReducer acc { text, file, attachment } = do
      -- Either legacy deprecated `file` or `attachment` must be set,
      -- both of them set is not allowed as both of them not set.
      guard $ let xor = notEq in isJust attachment `xor` isJust file
      snoc acc
        <$> { text, attachment: _ }
        <$> ((attachment <#> Modern) <|> (file <#> Legacy))

    answerReducer acc { nextSlide, header, text, file } =
      getSlide flatSlides nextSlide <#> \slide ->
        let x = { nextSlide: slide, header, text, file }
         in Map.insert nextSlide x acc
