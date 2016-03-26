define ["model/main", "text!tpl/partials/partnerDelayDialog.html"], (main, tpl) ->
  show: (caseId, svcId, partnerId) ->
    modelName = "PartnerDelay"

    $('body').append(
      Mustache.render tpl,
              title: "Опоздание партнёра"
              id: modelName
              saveLabel: "Зафиксировать"
              cancelLabel: "Отменить"
    )
    $modalDialog = $("##{modelName}-modal")

    $modalDialog
      .on('hidden.bs.modal', -> $modalDialog.remove())
      .on('show.bs.modal', ->
          obj =
            id: null
            caseId: caseId
            serviceId: svcId
            partnerId: partnerId
            owner: global.user.id

          kvm = main.modelSetup(modelName) "#{modelName}-form", obj,
            focusClass: "focusable"
            refs: []
            manual_save: true
      )

    $modalDialog.modal 'show'
