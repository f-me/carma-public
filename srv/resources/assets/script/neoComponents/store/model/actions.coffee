{Immutable: {Record}} = require "carma/vendor"
{makeActions} = require "carma/neoComponents/store/utils"

actions = makeActions __dirname,

  saveModelInstanceRequest:
    Payload: Record
      viewName: ""
      force: false

    handler: ({payload}) -> new Promise (resolve, reject) ->
      success = (kvm, model) -> resolve {kvm, model}

      failure = (jqXHR, kvm, model) ->
        err = new Error jqXHR.statusText
        Object.assign err, {jqXHR, kvm, model}
        reject err

      window.global.viewsWare[payload.get "viewName"].knockVM._meta.q
        .save success, payload.get("force"), failure

module.exports = actions
