define [ 'utils'
       , 'hotkeys'
       , 'text!tpl/screens/newCase.html'
       , 'model/utils'
       , 'model/main'
       ],
  (utils, hotkeys, tpl, mu, main) ->

    setupCaseMain = (viewName, args) ->
      kvm = main.modelSetup('case') viewName, args,
                         permEl       : 'case-permissions'
                         groupsForest : "center"
                         defaultGroup : "default-newCase"
                         slotsee      : ["case-number"]
                         focusClass   : 'focusable'
                         screenName   : 'newCase'
                         modelArg     : 'newCase'
                         hooks        : ['*']

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

      $('#go-back-to-call').on 'click', ->
        global.router.navigate "call", {trigger: true}

      $('#go-to-full-case').on 'click', ->
        kvm = global.viewsWare["case-form"].knockVM
        global.router.navigate "case/#{kvm.id()}", {trigger: true}

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
      $('#go-to-full-case').off 'click'
      $('body').off 'change.input'
      $('.navbar').css '-webkit-transform', ''

    makeCase = () ->
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
        city:                 v['city']()
        car_make:             v['carMake']()
        car_model:            v['carModel']()
        comment:              v['wazzup']()
        callTaker:            global.user.meta.realName
      main.buildNewModel 'case', args, {},
        (m, k) ->
          global.router.navigate("newCase/#{k.id()}", { trigger: true })


    addNewService = (name) ->
      kvm = global.viewsWare["case-form"].knockVM
      mu.addReference kvm,
        'services',
        {modelName : name, options: {modelArg: 'newCase', hooks: ['*']}},
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
