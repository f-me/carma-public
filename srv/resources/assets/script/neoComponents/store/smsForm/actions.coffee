{makeActions, catchFailure} = require "carma/neoComponents/store/utils"

actions = makeActions __dirname,
  showSmsForm: null
  closeSmsForm: null

  sendSmsFormRequest:
    ({payload: {caseId, phone, message, templateId}}, dispatch, getState) ->
      reqOpts =
        method: "POST"
        headers: {"Content-Type": "application/json"}
        credentials: "same-origin"
        body: JSON.stringify
          status:   "please-send"
          caseRef:  caseId
          phone:    phone
          template: templateId
          msgText:  message

      catchFailure dispatch, actions.sendSmsFormFailure,
        fetch "/_/Sms", reqOpts
          .then -> dispatch actions.sendSmsFormSuccess()

  sendSmsFormSuccess: null
  sendSmsFormFailure: null

module.exports = actions
