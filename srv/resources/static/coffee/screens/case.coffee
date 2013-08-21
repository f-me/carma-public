define [ "utils"
       , "hotkeys"
       , "text!tpl/screens/case.html"
       , "model/utils"
       , "model/main"
       ],
  (utils, hotkeys, tpl, mu, main) ->
    utils.build_global_fn 'pickPartnerBlip', ['map']

    # Case view (renders to #left, #center and #right as well)
    setupCaseMain = (viewName, args) -> setupCaseModel viewName, args

    setupCaseModel = (viewName, args) ->

      kvm = main.modelSetup("case") viewName, args,
                         permEl       : "case-permissions"
                         focusClass   : "focusable"
                         slotsee      : ["case-number"]
                         groupsForest : "center"
                         defaultGroup : "default-case"

      ctx = {fields: (f for f in kvm._meta.model.fields when f.meta?.required)}
      setCommentsHandler()

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

      do (kvm) ->
        ko.computed ->
          nots = (i for i of kvm when /.*Not$/.test i)
          if (_.any nots, (e) -> kvm[e]())
            k["resultDisabled"](true)  for k in kvm["actionsReference"]()
          else
            k["resultDisabled"](false) for k in kvm["actionsReference"]()

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
      mu.addReference kvm,
                   'services',
                   { modelName : name },
                   (k) ->
                      e = $('#' + k['view'])
                      e.parent().prev()[0].scrollIntoView()
                      e.find('input')[0].focus()

    utils.build_global_fn 'addService', ['screens/case']


    removeCaseMain = ->
      $("body").off "change.input"
      $('.navbar').css "-webkit-transform", ""

    { constructor       : setupCaseMain
    , destructor        : removeCaseMain
    , template          : tpl
    , addService        : addService
    }
