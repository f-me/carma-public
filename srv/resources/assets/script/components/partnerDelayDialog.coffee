define ["model/main", "text!tpl/partials/partnerDelayDialog.html"], (main, tpl) ->
  show: (svcKvm) ->
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
          kvm = main.modelSetup(modelName) "#{modelName}-form", {},
            focusClass: "focusable"
            refs: []
            manual_save: true

          footerKVM =
            save: ->
              kvm.caseId svcKvm.parentId()
              kvm.serviceId svcKvm.id()
              kvm.partnerId svcKvm.contractor_partnerId()
              kvm.owner global.user.id
              kvm._meta.q.save ->
                $modalDialog.modal 'hide'
                svcKvm._parent.renderActions()
            canSave: ko.pureComputed ->
              [kvm.delayReason(),
               (kvm.delayReason() != 1 or kvm.delayReasonComment()),
               kvm.delayMinutes(),
               kvm.notified(),
               kvm.delayConfirmed(),
               kvm.exceptional(),
               (kvm.exceptional() != 1 or kvm.exceptionalComment())
              ].every((x) -> !!x)
          footer = $("##{modelName}-modal .modal-footer")[0]
          ko.applyBindings(footerKVM, footer)
      )

    $modalDialog.modal 'show'
