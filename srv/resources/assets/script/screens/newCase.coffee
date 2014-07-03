define [ 'utils'
       , 'hotkeys'
       , 'text!tpl/screens/newCase.html'
       , 'model/utils'
       , 'model/main'
       , "components/contract"
       ],
  (utils, hotkeys, tpl, mu, main, Contract) ->

    setupCaseMain = (viewName, args) ->
      kaze = {}
      if args.id
        $.bgetJSON "/_/case/#{args.id}", (rsp) -> kaze = rsp

      kvm = main.modelSetup('case') viewName, args,
                         permEl       : 'case-permissions'
                         groupsForest : "center"
                         defaultGroup : "default-newCase"
                         slotsee      : ["case-number"]
                         focusClass   : 'focusable'
                         screenName   : 'newCase'
                         modelArg     : "ctr:new:#{kaze.program}"

      ctx = {fields: (f for f in kvm._meta.model.fields when f.meta?.required)}
      $("#empty-fields-placeholder").html(
          Mustache.render $("#empty-fields-template").html(), ctx)

      ko.applyBindings(kvm, el("empty-fields"))
      # Render service picker
      $("#service-picker-container").html(
        Mustache.render(
          $("#newService-picker-template").html(),
            {dictionary: global.dictionaries.Services
            ,drop: 'down'
            }))

      hotkeys.setup()
      Contract.setup "contract", kvm

      $('#go-back-to-call').on 'click', ->
        Finch.navigate "call"

      $('#go-back-and-transfer-to-bo').on 'click', ->
        kvm = global.viewsWare["case-form"].knockVM
        # FIXME!
        # FIXME! Possible race condition on the server: several triggers on
        # service.status will run in parallel and try to update case.actions.
        for svc in kvm.servicesReference()
          svc.status String global.idents("ServiceStatus")['backoffice']
        Finch.navigate "call"

      $('#go-to-full-case').on 'click', ->
        kvm = global.viewsWare["case-form"].knockVM
        Finch.navigate "case/#{kvm.id()}"

      $("#case-comments-b").on 'click', ->
        i = $("#case-comments-i")
        return if _.isEmpty i.val()
        comment =
          date: (new Date()).toString('dd.MM.yyyy HH:mm')
          user: global.user.login
          comment: i.val()
        k = global.viewsWare['case-form'].knockVM
        if _.isEmpty k['comments']()
          k['comments'] [comment]
        else
          k['comments'] k['comments']().concat comment
        i.val("")

    removeCaseMain = ->
      $('#go-back-to-call').off 'click'
      $('#go-back-and-transfer-to-bo').off 'click'
      $('#go-to-full-case').off 'click'
      $('body').off 'change.input'
      $('.navbar').css '-webkit-transform', ''

    makeCaseAux = () ->
      v = global.viewsWare['call-form'].knockVM

      callerType = v['callerType']()
      if callerType == 'client' or callerType == 'partner'
        v['callType']('newCase')
      else if not callerType
        v['callerType']('client')
        v['callType']('newCase')

      args =
        contact_name:         v['callerName_name']()
        contact_phone1:       v['callerName_phone1']()
        contact_phone2:       v['callerName_phone2']()
        contact_phone3:       v['callerName_phone3']()
        contact_phone4:       v['callerName_phone4']()
        contact_email:        v['callerName_email']()
        contact_contactOwner: v['callerName_contactOwner']()
        contact_ownerName:    v['callerName_ownerName']()
        contact_ownerPhone1:  v['callerName_ownerPhone1']()
        contact_ownerPhone2:  v['callerName_ownerPhone2']()
        contact_ownerPhone3:  v['callerName_ownerPhone3']()
        contact_ownerPhone4:  v['callerName_ownerPhone4']()
        contact_ownerEmail:   v['callerName_ownerEmail']()
        program:              v['program']()
        subprogram:           v['subprogram']()
        city:                 v['city']()
        car_make:             v['carMake']()
        car_model:            v['carModel']()
        caseAddress_coords:   v['coords']()
        caseAddress_address:  v['address']()
        comment:              v['wazzup']()
        customerComment:      v['customerComment']()
        callTaker:            global.user.meta.realName
      main.buildNewModel 'case', args, {modelArg: "ctr:full:#{v.program()}"},
        (m, k) -> Finch.navigate "newCase/#{k.id()}"

    makeCase = _.throttle makeCaseAux, 2000, {trailing: false}

    addNewService = (name) ->
      kvm = global.viewsWare["case-form"].knockVM
      modelArg = "ctr:new:#{kvm.program()}"
      mu.addReference kvm,
        'services',
        {modelName : name, options: {modelArg: modelArg, hooks: ['*']}},
        (k) ->
          e = $('#' + k['view'])
          e.parent().prev()[0]?.scrollIntoView()
          e.find('input')[0]?.focus()

    utils.build_global_fn 'addNewService', ['screens/newCase']

    { constructor       : setupCaseMain
    , destructor        : removeCaseMain
    , template          : tpl
    , addNewService     : addNewService
    , makeCase          : makeCase
    }