{Immutable: {Record}} = require "carma/vendor"

{makeActions, catchFailure, fetchPost} =
  require "carma/neoComponents/store/utils"

actions = makeActions __dirname,
  showSmsForm:
    Payload: Record
      phone: ""
      caseId: ""
      caseCity: ""
      caseAddress: ""

  closeSmsForm: null

  sendSmsFormRequest:
    Payload: Record
      caseId: ""
      phone: ""
      message: ""
      templateId: ""

    handler:
      ({payload}, dispatch) ->
        data =
          status:   "please-send"
          caseRef:  payload.get "caseId"
          phone:    payload.get "phone"
          template: payload.get "templateId"
          msgText:  payload.get "message"

        catchFailure dispatch, actions.sendSmsFormFailure, null,
          fetchPost "/_/Sms", data
            .then -> dispatch actions.sendSmsFormSuccess()

  sendSmsFormSuccess: null
  sendSmsFormFailure: null

module.exports = actions
