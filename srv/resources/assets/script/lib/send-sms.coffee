define [], ->
  div = document.getElementById("sms-send-form")
  renderForm = (props) ->
    ReactDOM.render(React.createElement(CarmaComponents.SmsForm, props), div)
  onHide = ->
    renderForm {isVisible: false, onHide: onHide, smsObj: {}}

  sendSms: ->
    vCase = global.viewsWare['case-form']
    smsObj = {
      caseRef: vCase?.knockVM.id(),
      phone: vCase?.knockVM.contact_phone1()}

    renderForm {isVisible: true, onHide: onHide, smsObj: smsObj}
