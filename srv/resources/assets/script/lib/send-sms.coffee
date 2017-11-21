{$} = require "carma/vendor"
{store} = require "carma/neoComponents/store"
{showSmsForm} = require "carma/neoComponents/store/smsForm/actions"

module.exports =
  sendSms: ->
    kase = window.global.viewsWare["case-form"]?.knockVM

    store.dispatch showSmsForm showSmsForm.Payload
      phone:       kase?.contact_phone1()      ? ""
      caseId:      kase?.id()                  ? ""
      caseCity:    kase?.cityLocal()           ? ""
      caseAddress: kase?.caseAddress_address() ? ""
