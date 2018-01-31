{Immutable: {Record}} = require "carma/vendor"

{makeActions, catchFailure, fetchPost, fetchGet} =
  require "carma/neoComponents/store/utils"

{SmsTemplateList} = require "./models"


formFlow =

  showSmsForm:
    Payload: Record
      phone       : ""
      caseId      : null # `null` or a number
      caseCity    : ""
      caseAddress : ""

    payloadCreator: (payload) ->
      validators =
        phone       : ["a string", (x) -> typeof x is "string"]
        caseId      : ["null or a number"
                       (x) -> x is null or
                         (typeof x is "number" and not isNaN x)]
        caseCity    : ["a string", (x) -> typeof x is "string"]
        caseAddress : ["a string", (x) -> typeof x is "string"]

      for k, [mustBe, f] of validators when not f payload.get k
        throw new Error "Field '#{k}' is invalid:
                         '#{payload.get k}' (#{typeof payload.get k}),
                         must be #{mustBe}"

      payload

  closeSmsForm: null


sendSmsFlow =

  sendSmsFormRequest:
    Payload: Record
      caseId     : null # `null` or a number
      phone      : ""
      message    : ""
      templateId : null # `null` or a number

    payloadCreator: (payload) ->
      validators =
        caseId     : ["null or a number"
                      (x) -> x is null or
                        (typeof x is "number" and not isNaN x)]
        phone      : ["a string", (x) -> typeof x is "string"]
        templateId : ["null or a number"
                      (x) -> x is null or
                        (typeof x is "number" and not isNaN x)]
        message    : ["a string", (x) -> typeof x is "string"]

      for k, [mustBe, f] of validators when not f payload.get k
        throw new Error "Field '#{k}' is invalid:
                         '#{payload.get k}' (#{typeof payload.get k}),
                         must be #{mustBe}"

      payload

    handler: ({payload}, dispatch) ->
      data =
        status   : "please-send"
        caseRef  : payload.get "caseId"
        phone    : payload.get "phone"
        template : payload.get "templateId"
        msgText  : payload.get "message"

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
