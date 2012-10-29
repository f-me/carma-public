
setTimeout(->
  $('#sms-send-modal').on('show', () ->
    refs = []
    modelSetup("sms") "sms-send-form", {id:null},
                          focusClass: "focusable"
                          refs: refs

    buttonDisabled = (dsbl) ->
      btn = $('#do-send-sms')
      if dsbl
        btn.attr('disabled', 'disabled')
      else
        btn.removeAttr('disabled')
          
    buttonDisabled true
    vSms = global.viewsWare['sms-send-form']
    smsVM = vSms.knockVM
    smsVM.msg.subscribe (msg) ->
      buttonDisabled (msg == "" || smsVM.phoneRegexp())

    smsVM.phoneRegexp.subscribe (err) ->
      buttonDisabled (err || smsVM.msg() == "")

    vCase = global.viewsWare['case-form']
    if vCase 
      smsVM.caseId(vCase.bbInstance.id)

    # we really need this because triggers do not trigger on `POST`
    # so, if {template:"xxx"} comes with POST (not with PUT), then
    # our template substitution trigger is not fired
    vSms.bbInstance.save()

    $('#do-send-sms')
      .off('click')
      .on('click', () ->
        $('#sms-send-modal').modal('hide')
        $.post('/smspost', {smsId: "sms:#{vSms.bbInstance.id}"})
        )
  )
,1000)
