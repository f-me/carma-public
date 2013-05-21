define ["model/main"], (main) ->

  # Function to init modal dialog
  # @param fnPartnerId function which returns id in format
  # "partner:{PARTNER_ID}"
  setup: (fnPartnerId) ->
    $(->
      modelName = "partnerCancel"
      $("##{modelName}-modal").on('show', () ->
        refs = []
        main.modelSetup("#{modelName}") "#{modelName}-form", {id:null},
          focusClass: "focusable"
          refs: refs

        vPartnerCancel = global.viewsWare["#{modelName}-form"]
        partnerCancelVM = vPartnerCancel.knockVM

        # make save button disabled until user choose a reason
        partnerCancelVM.partnerCancelReason.subscribe (reason) ->
          saveBtnDisable reason is ""

        saveBtnDisable = (disabled) ->
          $("##{modelName}-save").attr('disabled', disabled)

        saveBtnDisable true

        # fill hidden fields
        vCase = global.viewsWare['case-form']
        if vCase
          partnerCancelVM.caseId(vCase.bbInstance.id)

        partnerCancelVM.ctime(Math.round((new Date).getTime() / 1000))

        showAlert = (needShow) ->
          $alert = $("##{modelName}-alert-container")
          if needShow
            $alert.find(".alert-message").text("Забыли указать Партнёра в таблице?")
            $alert.show()
            $alert.find('.close').on('click', ->
              $alert.hide()
              $("##{modelName}-modal").modal('hide')
            )
          else
            $alert.hide()

        strPartnerId = fnPartnerId()
        if strPartnerId
          partnerId = parseInt strPartnerId.split(':')[1]
          partnerCancelVM.partnerId(partnerId)
          # hide alert
          showAlert false
        else
          # partnerId not defined
          # warn user about needed choose partner from table
          showAlert true

        # we really need this because triggers do not trigger on `POST`
        # so, if {template:"xxx"} comes with POST (not with PUT), then
        # our template substitution trigger is not fired
        vPartnerCancel.bbInstance.save()

        $("##{modelName}-save")
          .on('click', (event) ->
            event.preventDefault()
            $("##{modelName}-modal").modal('hide')

            # send data here
            # $.post('/partnerCancel',
            #   partnerCancelId: "partnerCancel:#{vPartnerCancel.bbInstance.id}"
            # )
          )
      )
    )
