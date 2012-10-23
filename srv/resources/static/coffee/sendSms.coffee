
setTimeout(->
  $('#sms-send-modal').on('show', () ->
    refs = []
    modelSetup("sms") "sms-send-form", {id:null},
                          focusClass: "focusable"
                          refs: refs

    vSms = global.viewsWare['sms-send-form']
    vSms.knockVM.phoneRegexp.subscribe (err) ->
      if err
        $('#do-send-sms').attr('disabled', 'disabled')
      else
        $('#do-send-sms').removeAttr('disabled')
      
    vCase = global.viewsWare['case-form']
    if vCase
      caze = vCase.bbInstance
      sms = vSms.knockVM
      sms.caseId(caze.id)
      sms.phone(caze.get('contact_phone1') || '')

    # we really need this because triggers do not trigger on `POST`
    # so, if {template:"xxx"} comes with POST (not wit PUT), then
    # out template substitution trigger is not fired
    vSms.bbInstance.save()

    $('#do-send-sms')
      .off('click')
      .on('click', () ->
        $('#sms-send-modal').modal('hide')
        )
  )
,1000)
