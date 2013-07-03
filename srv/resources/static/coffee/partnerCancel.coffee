define ["model/main"], (main) ->

  # Function to init modal dialog
  # @param fnPartnerId function which returns id in format
  # "partner:{PARTNER_ID}"
  # @param fnPartnerName function which returns partner name
  setup: (fnPartnerId, fnPartnerName) ->
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
        k = main.modelSetup("#{modelName}") "#{modelName}-form", {id:null},
          focusClass: "focusable"
          refs: refs
          bb: { maual_save: true }

        k['updateUrl'] = ->

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
          partnerCancelVM.caseId("case:#{vCase.knockVM.id()}")

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

        partnerId = fnPartnerId() if fnPartnerId
        if partnerId
          partnerCancelVM.partnerId(partnerId)
          # hide alert
          showAlert false
        else
          # partnerId not defined
          # warn user about needed choose partner from table
          showAlert true

        partnerCancelVM.owner(global.user.login)

        # write entry to comments history
        addToHistory = ->
          reasonCode = partnerCancelVM.partnerCancelReason()
          reason = _.find global.dictionaries.PartnerCancelReason.entries, (r) ->
            r.value is reasonCode
          comment =
            date: (new Date()).toString('dd.MM.yyyy HH:mm')
            user: global.user.login
            type: "Отказ партнёра"
            comment: fnPartnerName()
            result: "#{reason.label}"
          k = global.viewsWare['case-form'].knockVM
          if _.isEmpty k['comments']()
            k['comments'] [comment]
          else
            k['comments'] k['comments']().concat comment

        $("##{modelName}-save")
          .off('click')
          .on('click', (event) ->
            event.preventDefault()
            vPartnerCancel.bbInstance.save()
            do addToHistory

            $modalDialog.modal('hide')
          )
      )
      .on('hidden', () ->
        $(@).remove()
      )

    $modalDialog.modal 'show'
