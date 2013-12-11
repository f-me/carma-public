define [ "model/main"
         "model/utils"
         "utils"
         "text!tpl/screens/program.html"
         "screenman"
       ],
(main, mu, utils, tpl, screenman) ->

  modelSetup = (modelName, viewName, args) ->
    permEl = "#{modelName}-permissions"
    focusClass = "focusable"
    refs = []
    slotsee = ["#{modelName}-files"]
    options = {permEl, focusClass, refs, slotsee}
    main.modelSetup(modelName) viewName, args, options

  objsToRows = (objs) ->
    rows = for obj in objs
      [obj.id.split(':')[1]
      ,obj.label   || ''
      ]

  screenSetup = (viewName, args) ->
    modelName = "program"
    kvm = modelSetup modelName, viewName, args

    tableParams =
      tableName: "program"
      objURL: "/all/program"

    table = screenman.addScreen(modelName, -> )
      .addTable(tableParams)
      .setObjsToRowsConverter(objsToRows)
      .on("click.datatable", "tr", ->
        id = @children[0].innerText
        kvm = modelSetup modelName, viewName, {id}
        global.viewsWare["#{modelName}-view"].knockVM)
    screenman.showScreen modelName


    $('#program-permissions').find('.btn-success').on 'click', ->
      table.dataTable.fnAddData [[ kvm['id'](), kvm['label']() ]]

    $("#restore-defaults-btn").on 'click', ->
      $.ajax
        type: 'PUT'
        url: "/restoreProgramDefaults/#{kvm['id']()}"

  constructor: screenSetup
  destructor: ->
  template: tpl
