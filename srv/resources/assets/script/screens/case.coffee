define [ "utils"
       , "hotkeys"
       , "text!tpl/screens/case.html"
       , "model/utils"
       , "model/main"
       , "components/contract"
       ],
  (utils, hotkeys, tpl, mu, main, Contract) ->
    utils.build_global_fn 'pickPartnerBlip', ['map']

    # Case view (renders to #left, #center and #right as well)
    setupCaseMain = (viewName, args) -> setupCaseModel viewName, args

    setupCaseModel = (viewName, args) ->
      kaze = {}
      if args.id
        $.bgetJSON "/_/case/#{args.id}", (rsp) -> kaze = rsp

      kvm = main.modelSetup("case") viewName, args,
                         permEl       : "case-permissions"
                         focusClass   : "focusable"
                         slotsee      : ["case-number",
                                         "case-program-description",
                                         "case-car-description"]
                         groupsForest : "center"
                         defaultGroup : "default-case"
                         modelArg     : "ctr:full:#{kaze.program}"

      ctx = {fields: (f for f in kvm._meta.model.fields when f.meta?.required)}
      setCommentsHandler()

      Contract.setup "contract", kvm

      $("#empty-fields-placeholder").html(
          Mustache.render($("#empty-fields-template").html(), ctx))

      ko.applyBindings(kvm, el("empty-fields"))


      # Render service picker
      #
      # We use Bootstrap's glyphs if "icon" key is set in dictionary
      # entry.
      $("#service-picker-container").html(
        Mustache.render(
          $("#service-picker-template").html(),
            {dictionary: global.dictionaries["Services"]
            ,drop: 'up'
            }))

      $("body").on("change.input", ".redirectOnChange", () ->
          setTimeout(( -> window.location.hash = "back"), 500))

      utils.mkDataTable $('#call-searchtable')
      hotkeys.setup()
      kvm = global.viewsWare[viewName].knockVM

      if _.has kvm, 'actions'
        # Disable action results if any of required case fields is not
        # set
        do (kvm) ->
          ko.computed ->
            nots = (i for i of kvm when /.*Not$/.test i)
            disable = _.any nots, (e) -> kvm[e]()
            k.resultDisabled?(disable) for k in kvm.actionsReference?()

      # make colored services and actions a little bit nicer
      $('.accordion-toggle:has(> .alert)').css 'padding', 0

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


    # Top-level wrapper for storeService
    addService = (name) ->
      kvm = global.viewsWare["case-form"].knockVM
      modelArg = "ctr:full:#{kvm.program()}"
      mu.addReference kvm,
        'services',
        {modelName : name, options: {modelArg: modelArg, hooks: ['*']}},
        (k) ->
          e = $('#' + k['view'])
          e.parent().prev()[0]?.scrollIntoView()
          e.find('input')[0]?.focus()
          # make colored service a little bit nicer even if it is just created
          $('.accordion-toggle:has(> .alert)').css 'padding', 0

    utils.build_global_fn 'addService', ['screens/case']


    removeCaseMain = ->
      $("body").off "change.input"
      $('.navbar').css "-webkit-transform", ""


    { constructor       : setupCaseMain
    , destructor        : removeCaseMain
    , template          : tpl
    , addService        : addService
    }
