module App.Store.DiagTree.Editor.Handlers.SharedUtils.Slide
     ( getSlide
     , extractPartialBackendSlideFromSlide
     ) where

import Prelude hiding (id)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.MonadZero (guard)
import Control.Alt ((<|>))

import Data.Tuple (Tuple (Tuple))
import Data.Maybe (Maybe (..), maybe, isJust, isNothing)
import Data.JSDate (LOCALE, parse, toDateTime)
import Data.Map (Map)
import Data.Map as Map
import Data.Foldable (foldM)
import Data.Array (length, head, snoc, fromFoldable)

import Utils (toMaybeT)

import Utils.DiagTree.Editor
     ( diagTreeSlideActionFromBackend
     , diagTreeSlideActionToBackend
     )

import App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendSlide
     ( BackendSlide
     , PartialBackendSlide
     , defaultPartialBackendSlide
     )

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideId
     , DiagTreeSlide (DiagTreeSlide)
     , DiagTreeSlideAttachment (..)
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

  (Tuple answers answersIndexes) <-
    foldM answerReducer (Tuple [] Map.empty) s.answers

  -- "actions" must be an array of one element or empty array)
  action <- toMaybeT $ do
    let len = length actions
    guard $ len == 0 || len == 1

    if len == 1
       then head actions >>= diagTreeSlideActionFromBackend <#> Just
       else pure Nothing

  toMaybeT $ pure $ DiagTreeSlide
    { id, isRoot, ctime, header, body, resources, action
    , answers, answersIndexes
    }

  where
    resourceReducer acc { text, file, attachment } = do
      -- Either legacy deprecated `file` or `attachment` must be set,
      -- both of them set is not allowed as both of them not set.
      guard $ let xor = notEq in isJust attachment `xor` isJust file

      snoc acc
        <$> { text, attachment: _ }
        <$> ((attachment <#> Modern) <|> (file <#> Legacy))

    answerReducer
      (Tuple answers answersIndexes)
      { nextSlide: nextSlideId, header, text, attachment, file } = do

      -- Either legacy deprecated `file` or `attachment` must be set,
      -- both of them set at the same time is not allowed but both are optional.
      guard $ (isNothing attachment && isNothing file)
           || let xor = notEq in isJust attachment `xor` isJust file

      slide <- getSlide flatSlides nextSlideId

      let a = (attachment <#> Modern) <|> (file <#> Legacy)
          x = { nextSlide: slide, header, text, attachment: a }

      pure $ Tuple (answers `snoc` x)
           $ Map.insert nextSlideId (length answers) answersIndexes


extractPartialBackendSlideFromSlide :: DiagTreeSlide -> PartialBackendSlide
extractPartialBackendSlideFromSlide slide@(DiagTreeSlide s) =
  defaultPartialBackendSlide
    { header = Just s.header
    , body   = Just s.body

    , resources = Just $ s.resources <#> \x ->
        { text: x.text

        , attachment:
            case x.attachment of
                 Modern y -> Just y
                 _ -> Nothing

        , file:
            case x.attachment of
                 Legacy y -> Just y
                 _ -> Nothing
        }

    , actions = Just $
        maybe [] (\x -> [diagTreeSlideActionToBackend x]) s.action

    , answers = Just $ fromFoldable $
        s.answers <#> \x@{ nextSlide: (DiagTreeSlide nextS) } ->
          { nextSlide: nextS.id
          , header: x.header
          , text: x.text

          , attachment:
              case x.attachment of
                   Just (Modern y) -> Just y
                   _ -> Nothing

          , file:
              case x.attachment of
                   Just (Legacy y) -> Just y
                   _ -> Nothing
          }
    }
