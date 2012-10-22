
setTimeout(->
  $('#sms-send-modal').on('show', () ->
    refs = []
    modelSetup("sms") "sms-send-form", {id:null},
                          focusClass: "focusable"
                          refs: refs

    v1 = global.viewsWare['sms-send-form']
    v1.knockVM.phoneRegexp.subscribe (err) ->
      if err
        $('#do-send-sms').attr('disabled', 'disabled')
      else
        $('#do-send-sms').removeAttr('disabled')
      
    v2 = global.viewsWare['case-form']
    if v2
      i = v2.bbInstance
      j = v1.knockVM
      j.caseId(i.id)
      j.phone(i.get('contact_phone1') || '')

    $('#do-send-sms')
      .off('click')
      .on('click', () ->
        $('#sms-send-modal').modal('hide')
        alert "Sending"
        )
  )
,1000)
