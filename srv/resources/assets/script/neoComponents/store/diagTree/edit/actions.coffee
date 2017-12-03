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
            success = actions.loadSlidesSuccess
            dispatch success new success.Payload {slides}

  loadSlidesSuccess:
    Payload: Record
      slides: Map() # Map<number (slide id), SlideItem>

  loadSlidesFailure: null


actions = makeActions __dirname,
  Object.assign {},
    loadSlidesFlow

module.exports = actions
