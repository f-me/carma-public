define [ "model/main"
       , "fields/form.jade"]
       , (main, Fs) ->

  flds =  $('<div/>').append($(Fs()))
  class CancelDialog

    constructor: ->
      @callbacks =
        save: []

    # Function to init modal dialog
    # @param partnerId : int
    # @param serviceId : int
    # @param caseId    : int
    setup: (partnerId, serviceId, caseId) ->
      modelName = "PartnerCancel"

      $('body').append(
        Mustache.render $(flds).find("#modalDialog-field-template").html(),
                title: "Отказ партнёра"
                id: modelName
                saveLabel: "Сохранить"
                cancelLabel: "Отменить"
      )
      $modalDialog = $("##{modelName}-modal")

      $modalDialog
        .on('show.bs.modal', () =>
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

          kvm.owner(global.user.id)

          $("##{modelName}-save")
            .off('click')
            .on('click', (event) =>
              event.preventDefault()
              kvm._meta.q.save()
              $modalDialog.modal('hide')

              do cb for cb in @callbacks.save
            )
        )
        .on('hidden', () ->
          $(@).remove()
        )

      $modalDialog.modal 'show'

    onSave: (cb) ->
      @callbacks.save.push cb

  new CancelDialog
