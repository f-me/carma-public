define [ "text!tpl/screens/printSrv.html"
       , "model/main"
       , "sync/crud"
       ], (tpl, main, sync) ->

  setupPrintSrv = (viewName, {id: id}) ->
    $(".navbar").hide()

    svc = main.buildKVM global.model('Service'),
          fetched: {id: id}
          queue: sync.CrudQueue
    if svc.type() == global.idents("ServiceType").towage
      svc = main.buildKVM global.model('Towage'),
            fetched: {id: id}
            queueOptions: {hooks: ['*','Service']} # disable case-screen related hooks
            queue: sync.CrudQueue

    $.getJSON( "/_/Action?serviceId=#{id}&type=1" )
      .done((objs) ->
        svc.assignedTo = {realName: '-'}
        if objs?.length > 0
          ass  = _.last(_.sortBy objs, (o) -> o.closeTime).assignedTo
          if ass
            svc.assignedTo = main.buildKVM global.model('Usermeta'),
                fetched: {id: ass}
                queue: sync.CrudQueue

        kase = main.buildKVM global.model('Case'),
              fetched: {id: svc.parentId()}
              queue: sync.CrudQueue
        callTaker = main.buildKVM global.model('Usermeta'),
              fetched: {id: kase.callTaker()}
              queue: sync.CrudQueue
        cancels = ko.observableArray()
        $.getJSON( "/_/PartnerCancel?caseId=#{kase.id()}" )
          .done((objs) ->
            for obj in objs
              cancel = main.buildKVM global.model('PartnerCancel'),
                    fetched: obj
                    queue:   null
              owner = main.buildKVM global.model('Usermeta'),
                    fetched: {id: cancel.owner()}
                    queue: sync.CrudQueue
              partner = main.buildKVM global.model('Partner'),
                    fetched: {id: cancel.partnerId()}
                    queue: sync.CrudQueue
              service = main.buildKVM global.model('Service'),
                    fetched: {id: cancel.serviceId()}
                    queue: sync.CrudQueue
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

  { constructor: setupPrintSrv
  , destructor: destroyPrintSrv
  , template: tpl
  }
