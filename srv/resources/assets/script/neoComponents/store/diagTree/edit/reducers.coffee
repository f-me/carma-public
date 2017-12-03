{
  Immutable: {Record, Map}
  ReduxActions: {handleActions}
} = require "carma/vendor"

{} = require "./models"
actions = require "./actions"


DiagTreeEditState = Record
  slides: Map() # Map<number (slide id), SlideItem>
  selectedSlide: null # number (slide id)
  isSlidesLoading: false
  isSlidesLoaded: false
  isSlidesLoadingFailed: false


loadSlidesReducers =

  "#{actions.loadSlidesRequest}": (state, {payload}) -> state.merge
    slides: Map()
    selectedSlide: null
    isSlidesLoading: true
    isSlidesLoaded: false
    isSlidesLoadingFailed: false

  "#{actions.loadSlidesSuccess}": (state, {payload}) ->
    slides = payload.get "slides"

    state.merge {
      slides
      selectedSlide: slides.find((x) -> x.get "isRoot")?.get "id"
      isSlidesLoading: false
      isSlidesLoaded: true
      isSlidesLoadingFailed: false
    }

  "#{actions.loadSlidesFailure}": (state, {payload}) -> state.merge
    isSlidesLoading: false
    isSlidesLoaded: false
    isSlidesLoadingFailed: true


reducerMap = Object.assign {},
  loadSlidesReducers

module.exports = handleActions reducerMap, new DiagTreeEditState
