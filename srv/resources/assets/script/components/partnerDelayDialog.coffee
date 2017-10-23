{$, _, ko, Mustache} = require "carma/vendor"
{tpl} = require "carma/globallibs"

u    = require "carma/utils"
main = require "carma/model/main"
tpl  = tpl require "carma-tpl/partials/partnerDelayDialog.pug"

module.exports =
  show: (kase, svcKvm) ->
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
                # redirect to #back
                # This is the same behaviour as in kvm.buttons.cancel.click
                svcActs = u.svcActions kase, svcKvm,
                  [ global.idents("ActionType").orderService
                  , global.idents("ActionType").orderServiceAnalyst
                  ]
                if _.some(svcActs, (a) -> a.assignedTo() == global.user.id)
                  window.location.hash = "back"
                else
                  svcKvm._parent.renderActions()
                  svcKvm._parent.refreshHistory()
                  svcKvm._meta.q.fetch()
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
