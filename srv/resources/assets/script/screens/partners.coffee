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
      slotsee = ["map-address"]
      options = {permEl, focusClass, refs, slotsee}
      main.modelSetup(modelName) viewName, args, options

    objsToRows = (objs) ->
      cities = utils.newModelDict "City", true
      rows = for obj in objs
        [obj.id
        ,obj.name       || ''
        ,(cities.getLab obj.city) || ''
        ,obj.comment    || ''
        ]

    screenSetup = (viewName, args) ->
      modelName = "Partner"
      kvm = modelSetup modelName, viewName, args

      tableParams =
        tableName: "partner"
        objURL: "/_/Partner?limit=5000"

      table = screenman.addScreen(modelName, -> )
        .addTable(tableParams)
        .setObjsToRowsConverter(objsToRows)
      table
        .on("click.datatable", "tr", ->
          if (table.dataTable.fnGetPosition this) != null
            id = @children[0].innerText
            modelSetup modelName, viewName, {id}
        )
      screenman.showScreen modelName

      $('#partner-permissions').find('.btn-success').on 'click', ->
        obj =
          addrDeFacto:
                _.filter(kvm["addrsObjects"](),
                        (svm) -> svm.key() == "fact")[0]?.value()
          city: kvm.city()
          comment: kvm.comment()
          id: kvm.id()
          isDealer: kvm.isDealer()
          isMobile: kvm.isMobile()
          name: kvm.name()
        table.dataTable.fnAddData objsToRows [obj]


    { constructor: screenSetup
    , template: tpl
    }
