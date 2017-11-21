{Immutable: {Record}} = require "carma/vendor"
{makeActions, catchFailure} = require "carma/neoComponents/store/utils"

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
      ({payload}, dispatch, getState) ->
        reqOpts =
          method: "POST"
          headers: {"Content-Type": "application/json"}
          credentials: "same-origin"
          body: JSON.stringify
            status:   "please-send"
            caseRef:  payload.get "caseId"
            phone:    payload.get "phone"
            template: payload.get "templateId"
            msgText:  payload.get "message"

        catchFailure dispatch, actions.sendSmsFormFailure,
          fetch "/_/Sms", reqOpts
            .then -> dispatch actions.sendSmsFormSuccess()

  sendSmsFormSuccess: null
  sendSmsFormFailure: null

module.exports = actions
