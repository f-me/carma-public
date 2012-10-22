
setTimeout(->
  $('#sms-send-modal').on('show', () ->
    refs = []
    modelSetup("sms") "sms-send-form", {id:null},
                          focusClass: "focusable"
                          refs: refs
    $('#do-send-sms')
      .off('click')
      .on('click', () ->
        $('#sms-send-modal').modal('hide')
        alert "Sending"
        )
  )
,1000)
