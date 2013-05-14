define [ "model/main"
         "model/utils"
         "utils"
         "text!tpl/screens/program.html"
         "screenman"
       ],
(main, mu, utils, tpl, screenman) ->

  this.addNewPermissionToProgram = (name) ->
    p = global.viewsWare["program-view"].knockVM
    mu.addReference p,
                 'programPermissions',
                 {modelName: 'programPermissions'},
                 afterAddSrv(p)

  afterAddSrv = (parent) -> (k) -> utils.focusRef k

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

    f = $("#program-files").html()

    tableParams =
      tableName: "program"
      objURL: "/all/program"

    table = screenman.addScreen(modelName, -> )
      .addTable(tableParams)
      .setObjsToRowsConverter(objsToRows)
      .on("click.datatable", "tr", ->
        $("#program-files").html(f)
        id = @children[0].innerText
        modelSetup modelName, viewName, {id}
        global.viewsWare["#{modelName}-view"].knockVM)
    screenman.showScreen modelName


    $('#program-permissions').find('.btn-success').on 'click', ->
      table.dataTable.fnAddData [[ kvm['id'](), kvm['label']() ]]

    $("#program-add-programPermissions-container").html(
      Mustache.render $("#add-ref-button-template").html(),
              fn:    "addNewPermissionToProgram();"
              label: "Добавить ограничение на поле контракта"
    )

  constructor: screenSetup
  destructor: ->
  template: tpl
