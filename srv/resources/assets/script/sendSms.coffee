define ["model/main"], (main) ->
  setup: -> setTimeout(->
    $('#sms-send-modal').on('show.bs.modal', () ->
      refs = []
      main.modelSetup("Sms") "sms-send-form", {id:null},
                            focusClass: "focusable"
                            refs: refs
                            manual_save: true

      buttonDisabled = (dsbl) ->
        btn = $('#do-send-sms')
        if dsbl
          btn.attr('disabled', 'disabled')
        else
          btn.removeAttr('disabled')

      buttonDisabled true
      vSms = global.viewsWare['sms-send-form']
      smsVM = vSms.knockVM
      smsVM['updateUrl'] = ->
      smsVM.msgText.subscribe (msg) ->
        buttonDisabled (msg == "" || smsVM.phoneRegexp())

      smsVM.phoneRegexp.subscribe (err) ->
        buttonDisabled (err || smsVM.msgText() == "")

      vCase = global.viewsWare['case-form']
      if vCase
        smsVM.caseRef(vCase.knockVM.id())
        smsVM.phone(vCase.knockVM.contact_phone1())

      # FIXME: now we can do this with shiny new triggers
      # we really need this because triggers do not trigger on `POST`
      # so, if {template:"xxx"} comes with POST (not with PUT), then
      # our template substitution trigger is not fired
      smsVM._meta.q.save()

      $('#do-send-sms')
        .off('click')
        .on('click', () ->
          $('#sms-send-modal').modal('hide')
          smsVM.status 'please-send'
          smsVM._meta.q.save()
          )
    )
  ,1000)
