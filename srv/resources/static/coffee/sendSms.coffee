
setTimeout(->
  $('#sms-send-modal').on('show', () ->
    refs = []
    modelSetup("sms") "sms-send-form", {id:null},
                          focusClass: "focusable"
                          refs: refs

    v1 = global.viewsWare['case-form']
    if v1
      i = v1.bbInstance
      j = global.viewsWare['sms-send-form'].knockVM
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
