
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

    vSms.knockVM.caseId.subscribe (caseId) ->
      if caseId
        $.ajax
          url: "/_/case/#{caseId}"
          dataType: "json"
          async: false
          success: (caze) ->
            phone = caze.contact_phone1
            phone && vSms.knockVM.phone(phone)
      
    vCase = global.viewsWare['case-form']
    if vCase 
      vSms.knockVM.caseId(vCase.bbInstance.id)

    # we really need this because triggers do not trigger on `POST`
    # so, if {template:"xxx"} comes with POST (not wit PUT), then
    # out template substitution trigger is not fired
    vSms.bbInstance.save()

    $('#do-send-sms')
      .off('click')
      .on('click', () ->
        $('#sms-send-modal').modal('hide')
        $.post('/smspost', {smsId: "sms:#{vSms.bbInstance.id}"})
        )
  )
,1000)
