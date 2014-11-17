define [ "utils"
       , "hotkeys"
       , "text!tpl/screens/case.html"
       , "text!tpl/fields/form.html"
       , "model/utils"
       , "model/main"
       , "components/contract"
       ],
  (utils, hotkeys, tpl, Flds, mu, main, Contract) ->
    utils.build_global_fn 'pickPartnerBlip', ['map']

    flds =  $('<div/>').append($(Flds))
    # Case view (renders to #left, #center and #right as well)
    setupCaseMain = (viewName, args) -> setupCaseModel viewName, args

    setupCaseModel = (viewName, args) ->
      kaze = {}
      # Bootstrap case data to load proper view for Case model
      # depending on the program
      if args.id
        $.bgetJSON "/_/Case/#{args.id}", (rsp) -> kaze = rsp

      kvm = main.modelSetup("Case") viewName, args,
                         permEl       : "case-permissions"
                         focusClass   : "focusable"
                         slotsee      : ["case-number",
                                         "case-program-description",
                                         "case-car-description"]
                         groupsForest : "center"
                         defaultGroup : "default-case"
                         modelArg     : "ctr:#{kaze.program}"

      # TODO The server should notify the client about new actions
      # appearing in the case instead of explicit subscription
      kvm["caseStatusSync"]?.subscribe (nv) ->
        if !nv
          kvm['renderActions']?()

      ctx = {fields: (f for f in kvm._meta.model.fields when f.meta?.required)}
      setCommentsHandler()

      Contract.setup "contract", kvm

      $("#empty-fields-placeholder").html(
          Mustache.render($(flds).find("#empty-fields-template").html(), ctx))

      ko.applyBindings(kvm, el("empty-fields"))


      # Render service picker
      #
      # We use Bootstrap's glyphs if "icon" key is set in dictionary
      # entry.
      $("#service-picker-container").html(
        Mustache.render(
          $(flds).find("#service-picker-template").html(),
            {dictionary: utils.newComputedDict("iconizedServiceTypes")
            ,drop: 'up'
            }))

      utils.mkDataTable $('#call-searchtable')
      hotkeys.setup()
      kvm = global.viewsWare[viewName].knockVM

      # True if any of of required fields are missing a value
      do (kvm) ->
        kvm['hasMissingRequireds'] = ko.computed ->
          nots = (i for i of kvm when /.*Not$/.test i)
          disable = _.any nots, (e) -> kvm[e]()
          disable

      kvm['renderActions'] = -> renderActions(kvm)
      kvm['renderActions']()

      # make colored services and actions a little bit nicer
      $('.accordion-toggle:has(> .alert)').css 'padding', 0

      $(".status-btn-tooltip").tooltip()

    setCommentsHandler = ->
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

    # Manually re-render a list of case actions
    #
    # TODO Implement this as a read trigger for Case.actions EF with
    # WS subscription to action list updates
    renderActions = (kvm) ->
      caseId = kvm.id()
      refCounter = 0

      mkSubname = -> "case-#{caseId}-actions-view-#{refCounter++}"
      subclass = "case-#{caseId}-actions-views"

      # Top-level container for action elements
      cont = $("#case-actions-list")

      # TODO Add garbage collection
      # $(".#{subclass}").each((i, e) ->
      #   main.cleanupKVM global.viewsWare[e.id].knockVM)
      cont.children().remove()
      cont.spin('large')

      # Pick reference template
      tpl = flds.find("#actions-reference-template").html()

      # Flush old actionsList
      if kvm['actionsList']?
        kvm['actionsList'].removeAll()

      $.getJSON "/backoffice/caseActions/#{caseId}", (aids) ->
        for aid in aids
          # Generate reference container
          view = mkSubname()
          box = Mustache.render tpl,
            refView: view
            refClass: subclass
          cont.append box
          avm = main.modelSetup("Action") view, {id: aid},
            slotsee: [view + "-link"]
            parent: kvm
          # Redirect to backoffice when an action result changes
          avm["resultSync"]?.subscribe (nv) ->
            if !nv
              window.location.hash = "back"
          # There's no guarantee who renders first (services or
          # actions), try to set up an observable from here
          if not kvm['actionsList']?
            kvm['actionsList'] = ko.observableArray()
          kvm['actionsList'].push avm
          # Disable action results if any of required case fields is
          # not set
          do (avm) ->
            avm.resultDisabled kvm['hasMissingRequireds']()
            kvm['hasMissingRequireds'].subscribe (dis) ->
              avm.resultDisabled?(dis)
        cont.spin false
      kvm['fillEventHistory']?()

    # Top-level wrapper for storeService
    addService = (name) ->
      kvm = global.viewsWare["case-form"].knockVM
      modelArg = "ctr:#{kvm.program()}"
      mu.addReference kvm,
        'services',
        {modelName : name, options:
          newStyle: true
          parentField: 'parentId'
          modelArg: modelArg
          hooks: ['*']},
        (k) ->
          e = $('#' + k['view'])
          e.parent().prev()[0]?.scrollIntoView()
          e.find('input')[0]?.focus()
          # make colored service a little bit nicer even if it is just created
          $('.accordion-toggle:has(> .alert)').css 'padding', 0
          $(".status-btn-tooltip").tooltip()
          e.parent().collapse 'show'

    utils.build_global_fn 'addService', ['screens/case']


    removeCaseMain = ->
      $("body").off "change.input"
      $('.navbar').css "-webkit-transform", ""

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
      main.buildNewModel 'Case', args, {modelArg: "ctr:#{v.program()}"},
        (m, k) -> Finch.navigate "case/#{k.id()}"

    makeCase = _.throttle makeCaseAux, 2000, {trailing: false}

    { constructor       : setupCaseMain
    , destructor        : removeCaseMain
    , template          : tpl
    , addService        : addService
    , makeCase          : makeCase
    }
