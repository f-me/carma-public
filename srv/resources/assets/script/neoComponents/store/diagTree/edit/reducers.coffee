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
  isParsingSlidesDataFailed: false


loadSlidesReducers =

  "#{actions.loadSlidesRequest}": (state, {payload}) -> state.merge
    slides                    : Map()
    selectedSlide             : null
    isSlidesLoading           : true
    isSlidesLoaded            : false
    isSlidesLoadingFailed     : false
    isParsingSlidesDataFailed : false

  "#{actions.loadSlidesSuccess}": (state, {payload}) -> state.merge
    slides          : payload.get "slides"
    selectedSlide   : payload.get "rootSlide"
    isSlidesLoading : false
    isSlidesLoaded  : true

  "#{actions.loadSlidesFailure}": (state, {payload}) ->
    state.withMutations (s) ->
      if payload?.message?.indexOf("Root slide not found") isnt -1
        s.set "isParsingSlidesDataFailed", true

      s.merge
        isSlidesLoading       : false
        isSlidesLoadingFailed : true


reducerMap = Object.assign {},
  loadSlidesReducers

module.exports = handleActions reducerMap, new DiagTreeEditState
