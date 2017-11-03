{$} = require "carma/vendor"

smsTemplates = []

div = document.getElementById 'sms-send-form'
renderForm = (props) ->
  ReactDOM.render(React.createElement(CarmaComponents.SmsForm, props), div)

onHide = ->
  renderForm {isVisible: false, onHide: onHide}


module.exports =

  setupSmsForm: ->
    $.ajax
      type: 'GET'
      url: '/_/SmsTemplate'
      dataType: 'json'
      success: (res) ->
        smsTemplates = res.filter((x) => x.isActive)

  sendSms: ->
    kase = window.global.viewsWare['case-form']?.knockVM
    renderForm
      isVisible: true
      onHide: onHide
      smsTemplates: smsTemplates
      values:
        phone: kase?.contact_phone1(),
        'case.id': kase?.id(),
        'case.city': kase?.cityLocal(),
        'case.caseAddress_address': kase?.caseAddress_address()
