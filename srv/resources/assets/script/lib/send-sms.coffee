define [], ->
  smsTemplates = []

  div = document.getElementById 'sms-send-form'
  renderForm = (props) ->
    ReactDOM.render(React.createElement(CarmaComponents.SmsForm, props), div)

  onHide = ->
    renderForm {isVisible: false, onHide: onHide}

  setupSmsForm: ->
    $.ajax
      type: 'GET'
      url: '/_/SmsTemplate'
      dataType: 'json'
      success: (res) ->
        smsTemplates = res.filter((x) => x.isActive)

  sendSms: ->
    vCase = global.viewsWare['case-form']
    renderForm
      isVisible: true
      onHide: onHide
      smsTemplates: smsTemplates
      defaultValues:
        caseRef: vCase?.knockVM.id(),
        phone: vCase?.knockVM.contact_phone1()
