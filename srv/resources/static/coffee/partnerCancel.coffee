define ["model/main"], (main) ->

  # Function to init modal dialog
  # @param partnerId : int
  # @param serviceId : String - in format "#{serviceName}:#{serviceId}"
  # @param caseId    : String - in format "case:#{caseId}"
  setup: (partnerId, serviceId, caseId) ->
    modelName = "partnerCancel"

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
        kvm = main.modelSetup("#{modelName}") "#{modelName}-form", {id:null},
          focusClass: "focusable"
          refs: refs
          manual_save: true

        kvm['updateUrl'] = ->

        # make save button disabled until user choose a reason
        kvm.partnerCancelReason.subscribe (reason) ->
          saveBtnDisable reason is ""

        saveBtnDisable = (disabled) ->
          $("##{modelName}-save").attr('disabled', disabled)

        saveBtnDisable true

        # fill hidden fields
        kvm.serviceId(serviceId)

        kvm.caseId(caseId)

        ctime = Math.round((new Date).getTime() / 1000)
        kvm.ctime("#{ctime}")

        showAlert = (needShow) ->
          $alert = $("##{modelName}-alert-container")
          if needShow
            $alert.find(".alert-message").text("Забыли указать Партнёра?")
            $alert.show()
            $alert.find('.close').on('click', ->
              $alert.hide()
              $modalDialog.modal('hide')
            )
          else
            $alert.hide()

        if partnerId
          kvm.partnerId(partnerId)
          # hide alert
          showAlert false
        else
          # partnerId not defined
          # warn user about needed choose partner from table
          showAlert true

        kvm.owner(global.user.login)

        $("##{modelName}-save")
          .off('click')
          .on('click', (event) ->
            event.preventDefault()
            kvm._meta.q.save()
            $modalDialog.modal('hide')
          )
      )
      .on('hidden', () ->
        $(@).remove()
      )

    $modalDialog.modal 'show'
