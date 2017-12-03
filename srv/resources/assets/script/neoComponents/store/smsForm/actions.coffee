{Immutable: {Record}} = require "carma/vendor"

{makeActions, catchFailure, fetchPost, fetchGet} =
  require "carma/neoComponents/store/utils"

{SmsTemplateList} = require "./models"


formFlow =

  showSmsForm:
    Payload: Record
      phone: ""
      caseId: ""
      caseCity: ""
      caseAddress: ""

  closeSmsForm: null


sendSmsFlow =

  sendSmsFormRequest:
    Payload: Record
      caseId: ""
      phone: ""
      message: ""
      templateId: ""

    handler: ({payload}, dispatch) ->
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


loadSmsTemplatesFlow =

  loadSmsTemplatesRequest:
    handler: ({payload}, dispatch) ->
      catchFailure dispatch, actions.loadSmsTemplatesFailure, null,
        fetchGet "/_/SmsTemplate"
          .then (smsTemplates) -> SmsTemplateList.fromPlain smsTemplates
          .then (smsTemplates) ->
            success = actions.loadSmsTemplatesSuccess
            dispatch success new success.Payload {smsTemplates}

  loadSmsTemplatesSuccess:
    Payload: Record
      smsTemplates: new SmsTemplateList

  loadSmsTemplatesFailure: null


actions = makeActions __dirname,
  Object.assign {},
    formFlow,
    sendSmsFlow,
    loadSmsTemplatesFlow

module.exports = actions
