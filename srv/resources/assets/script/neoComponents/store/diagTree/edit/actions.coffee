{Immutable: {Record, Map}} = require "carma/vendor"

{makeActions, catchFailure, fetchGet, fetchPost, fetchPut} =
  require "carma/neoComponents/store/utils"

{SlideItem} = require "./models"


loadSlidesFlow =

  loadSlidesRequest:
    handler: (action, dispatch) ->
      catchFailure dispatch, actions.loadSlidesFailure, null,
        fetchGet "/_/DiagSlide"
          .then (plainArr) ->
            Map().withMutations (m) ->
              m.set slide.id, SlideItem.fromPlain slide for slide in plainArr
          .then (slides) ->
            rootSlide = slides.find((x) -> x.get "isRoot")?.get "id"
            throw new Error "Root slide not found" unless rootSlide?
            success = actions.loadSlidesSuccess
            dispatch success new success.Payload {slides, rootSlide}

  loadSlidesSuccess:
    Payload: Record
      slides: Map() # Map<number (slide id), SlideItem>
      rootSlide: 0 # Slide id

  loadSlidesFailure: null


actions = makeActions __dirname,
  Object.assign {},
    loadSlidesFlow

module.exports = actions
