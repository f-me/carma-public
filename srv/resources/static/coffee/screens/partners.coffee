define [ "utils"
       , "text!tpl/screens/partner.html"
       , "model/utils"
       , "model/main"
       , "screenman"
       ],
  (utils, tpl, mu, main, screenman) ->

    modelSetup = (modelName, viewName, args) ->
      permEl = "#{modelName}-permissions"
      focusClass = "focusable"
      refs = [field: "services"
             ,forest: "partner-services-references"
             ]
      options = {permEl, focusClass, refs}
      main.modelSetup(modelName) viewName, args, options

    objsToRows = (objs) ->
      dict = global.dictValueCache['DealerCities']
      rows = for obj in objs
        [obj.id
        ,obj.name       || ''
        ,dict[obj.city] || obj.city
        ,obj.comment    || ''
        ]

    screenSetup = (viewName, args) ->
      modelName = "partner"
      kvm = modelSetup modelName, viewName, args

      utils.build_global_fn 'addNewServiceToPartner', ['screens/partners']
      $("#partner-add-service-container").html(
        Mustache.render $("#add-ref-button-template").html(),
                fn:    "addNewServiceToPartner();"
                label: "Добавить услугу"
      )

      # I need this object because I can't clean foreach binding, once
      # it's created, to use this proxy object to keep current partner's
      # allerts
      global.alertObj = { kvm: ko.observable(kvm)}
      ko.applyBindings(global.alertObj, $("#partner-errors")[0])

      tableParams =
        tableName: "partner"
        objURL: "/allPartners"

      screenman.addScreen(modelName, -> )
        .addTable(tableParams)
        .setObjsToRowsConverter(objsToRows)
        .on("click.datatable", "tr", ->
          id = @children[0].innerText
          kvm = modelSetup modelName, viewName, {id}
          k = global.viewsWare["#{modelName}-view"].knockVM
          global.alertObj.kvm(kvm)

          k['servicesReference'].subscribe ->
            addTarifStuff i for i in k['servicesReference']())
      screenman.showScreen modelName

    addNewServiceToPartner = (name) ->
      p = global.viewsWare["partner-view"].knockVM
      mu.addReference p,
                   'services',
                   {modelName: 'partner_service'},
                   afterAddSrv(p)

    afterAddSrv = (parent) -> (k) ->
      utils.focusRef k

    addTarifStuff = (p) ->
      view = $("##{p['view']}")
      button = Mustache.render $("#add-ref-button-template").html(),
        fn:    ""
        label: "Добавить опцию"
      view.children().last().after button
      view.children().last().click -> genNewTarif p

    genNewTarif = (kvm) ->
      mu.addReference kvm, 'tarifOptions', { modelName: 'tarifOption' },
        (k) ->
          utils.bindRemove kvm, 'tarifOptions'
          utils.focusRef k

    screenRelease = () ->
      ko.cleanNode($("#partner-errors")[0])
      delete global.alertObj

    { constructor: screenSetup
    , destructor : screenRelease
    , template: tpl
    , addNewServiceToPartner: addNewServiceToPartner
    }
