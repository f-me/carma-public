{$} = require "carma/vendor"
{baseModel} = require "carma/neoComponents/BaseModel"

module.exports =
  sendSms: ->
    kase = window.global.viewsWare['case-form']?.knockVM

    baseModel.showSmsForm
      phone:       kase?.contact_phone1()      ? ''
      caseId:      kase?.id()                  ? ''
      caseCity:    kase?.cityLocal()           ? ''
      caseAddress: kase?.caseAddress_address() ? ''
