define ["model/main"], (main) ->

  # Function to init modal dialog
  # @param kvm is service knockout model
  setup: (kvm) ->
    modelName = "partnerCancel"
    partnerId   = kvm.contractor_partnerId()
    partnerName = kvm.contractor_partner()
    serviceId   = "#{kvm._meta.model.name}:#{kvm.id()}"

    $('body').append(
      Mustache.render $("#modalDialog-field-template").html(),
              title: "Отказ партнёра"
              id: modelName
              saveLabel: "Сохранить"
              cancelLabel: "Отменить"
    )
    $modalDialog = $("##{modelName}-modal")

    $modalDialog
      .on('show', () ->
        refs = []
        k = main.modelSetup("#{modelName}") "#{modelName}-form", {id:null},
          focusClass: "focusable"
          refs: refs
          manual_save: true

        k['updateUrl'] = ->

        vPartnerCancel = global.viewsWare["#{modelName}-form"]
        partnerCancelVM = vPartnerCancel.knockVM

        # make save button disabled until user choose a reason
        partnerCancelVM.partnerCancelReason.subscribe (reason) ->
          saveBtnDisable reason is ""

        saveBtnDisable = (disabled) ->
          $("##{modelName}-save").attr('disabled', disabled)

        saveBtnDisable true

        partnerCancelVM.serviceId(serviceId)
        # fill hidden fields
        vCase = global.viewsWare['case-form']?.knockVM
        if vCase
          partnerCancelVM.caseId("case:#{vCase.id()}")

        ctime = Math.round((new Date).getTime() / 1000)
        partnerCancelVM.ctime("#{ctime}")

        showAlert = (needShow) ->
          $alert = $("##{modelName}-alert-container")
          if needShow
            $alert.find(".alert-message").text("Забыли указать Партнёра в таблице?")
            $alert.show()
            $alert.find('.close').on('click', ->
              $alert.hide()
              $modalDialog.modal('hide')
            )
          else
            $alert.hide()

        if partnerId
          partnerCancelVM.partnerId(partnerId)
          # hide alert
          showAlert false
        else
          # partnerId not defined
          # warn user about needed choose partner from table
          showAlert true

        partnerCancelVM.owner(global.user.login)

        $("##{modelName}-save")
          .off('click')
          .on('click', (event) ->
            event.preventDefault()
            partnerCancelVM._meta.q.save()
            $modalDialog.modal('hide')
          )
      )
      .on('hidden', () ->
        $(@).remove()
      )

    $modalDialog.modal 'show'
