define [ "utils"
       , "hotkeys"
       , "text!tpl/screens/case.html"
       , "model/utils"
       , "model/main"
       , "sync/datamap"
       ],
  (utils, hotkeys, tpl, mu, main, dm) ->
    utils.build_global_fn 'pickPartnerBlip', ['map']

    # Case view (renders to #left, #center and #right as well)
    setupCaseMain = (viewName, args) -> setupCaseModel viewName, args

    setupCaseModel = (viewName, args) ->

      kvm = main.modelSetup("case") viewName, args,
                         permEl       : "case-permissions"
                         focusClass   : "focusable"
                         slotsee      : ["case-number", "case-program-description"]
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

      if utils.canReadActions()
        # Disable action results if any of required case fields is not
        # set
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

    # Load case data from contract, possibly ignoring a set of given
    # case fields
    loadContract = (cid, ignored_fields) ->
      date = (v) -> dm.s2c v, "date"
      fieldMap =
        [ { from: "carVin", to: "car_vin" }
        , { from: "carMake", to: "car_make" }
        , { from: "carModel", to: "car_model" }
        , { from: "carPlateNum", to: "car_plateNum" }
        , { from: "carColor", to: "car_color" }
        , { from: "carTransmission", to: "car_transmission" }
        , { from: "carEngine", to: "car_engine" }
        , { from: "contractType", to: "car_contractType" }
        , { from: "carCheckPeriod", to: "car_checkPeriod" }
        , { from: "carBuyDate", to: "car_buyDate", proj: date }
        , { from: "carCheckupDate", to: "car_checkupDate", proj: date }
        , { from: "carCheckupMilage", to: "car_checkupMileage" }
        , { from: "milageTO", to: "cardNumber_milageTO" }
        , { from: "cardNumber", to: "cardNumber_cardNumber" }
        , { from: "carMakeYear", to: "car_makeYear" }
        , { from: "contractValidUntilMilage", to: "cardNumber_validUntilMilage" }
        , { from: "contractValidFromDate", to: "cardNumber_validFrom", proj: date }
        , { from: "contractValidUntilDate", to: "cardNumber_validUntil", proj: date }
        , { from: "warrantyStart", to: "car_warrantyStart", proj: date }
        , { from: "warrantyEnd", to: "car_warrantyEnd", proj: date }
        , { from: "carSeller", to: "car_seller" }
        , { from: "carDealerTO", to: "car_dealerTO" }
        ]

      kvm = global.viewsWare["case-form"].knockVM
      
      $.getJSON "/_/contract/#{cid}", (res) ->
        for field in _.filter fieldMap, ((f) -> !_.contains ignored_fields, f.to)
          do (field) ->
            # Do not splice unknown contract fields, do not overwrite
            # existing case fields
            if res[field.from]? && _.isEmpty kvm[field.to]()
              if field.proj?
                kvm[field.to] field.proj res[field.from]
              else
                kvm[field.to] res[field.from]

    loadContractCard = (cid) -> loadContract cid, ["cardNumber_cardNumber"]

    # Globalize loader so that cards-dict can use it
    utils.build_global_fn 'loadContractCard', ['screens/case']

    { constructor       : setupCaseMain
    , destructor        : removeCaseMain
    , template          : tpl
    , addService        : addService
    , loadContractCard  : loadContractCard
    }
