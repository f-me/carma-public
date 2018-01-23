{$, _, ko} = require "carma/vendor"

{CrudQueue} = require "carma/sync/crud"
main        = require "carma/model/main"
template    = require "carma-tpl/screens/printSrv.pug"

setupPrintSrv = (viewName, {id}) ->
  $(".navbar").hide()

  svc = main.buildKVM window.global.model('Service'),
        fetched: {id: id}
        queue: CrudQueue
  if svc.type() == window.global.idents("ServiceType").towage
    svc = main.buildKVM window.global.model('Towage'),
          fetched: {id: id}
          queueOptions: {hooks: ['*','Service']} # disable case-screen related hooks
          queue: CrudQueue

  $.getJSON( "/_/Action?serviceId=#{id}&type=1" )
    .done((objs) ->
      svc.assignedTo = {realName: '-'}
      if objs?.length > 0
        ass  = _.last(_.sortBy objs, (o) -> o.closeTime).assignedTo
        if ass
          svc.assignedTo = main.buildKVM window.global.model('Usermeta'),
              fetched: {id: ass}
              queue: CrudQueue

      kase = main.buildKVM window.global.model('Case'),
            fetched: {id: svc.parentId()}
            queue: CrudQueue
      callTaker = main.buildKVM window.global.model('Usermeta'),
            fetched: {id: kase.callTaker()}
            queue: CrudQueue
      cancels = ko.observableArray()
      $.getJSON( "/_/PartnerCancel?caseId=#{kase.id()}" )
        .done((objs) ->
          for obj in objs
            cancel = main.buildKVM window.global.model('PartnerCancel'),
                  fetched: obj
                  queue:   null
            owner = main.buildKVM window.global.model('Usermeta'),
                  fetched: {id: cancel.owner()}
                  queue: CrudQueue
            partner = main.buildKVM window.global.model('Partner'),
                  fetched: {id: cancel.partnerId()}
                  queue: CrudQueue
            service = main.buildKVM window.global.model('Service'),
                  fetched: {id: cancel.serviceId()}
                  queue: CrudQueue
            cancels.push(
              ctime:   new Date(cancel.ctime()).toString("dd.MM.yyyy HH:mm")
              owner:   owner.realName()
              partner: partner.name()
              reason:  cancel.partnerCancelReasonLocal()
              comment: cancel.comment() || ''
              service: service.typeLocal()
            )
        )
      comments = ko.observableArray()

      $.getJSON("/caseHistory/#{kase.id()}")
        .done((objs) ->
          for obj in objs
            if obj[2].commenttext?
              comments.push(
                user: obj[1]
                date: new Date(obj[0]).toString "dd.MM.yyyy HH:mm:ss"
                comment: obj[2].commenttext
              )
        )

      program = kase.programLocal()
      if kase.subprogramLocal()
        program += ' / ' + kase.subprogramLocal()
      kvm =
        kase: kase
        service: svc
        callTaker: callTaker
        comments: comments
        program: program
        cancels: cancels
      ko.applyBindings kvm, el("print-table")
  )

destroyPrintSrv = () ->
  $(".navbar").show()

module.exports =
  { constructor: setupPrintSrv
  , destructor: destroyPrintSrv
  , template
  }
