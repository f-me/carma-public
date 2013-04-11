define [ "model/main"
         "model/utils"
         "utils"
         "text!tpl/screens/program.html"
       ],
(main, mu, utils, tpl) ->
  this.addNewPermissionToProgram = (name) ->
    p = global.viewsWare["program-view"].knockVM
    mu.addReference p,
                 'programPermissions',
                 {modelName: 'programPermissions'},
                 afterAddSrv(p)

  afterAddSrv = (parent) -> (k) -> utils.focusRef k

  constructor: (viewName, args) ->
    f = $("#program-files").html()
    kvm = main.modelSetup("program") viewName, args,
                          permEl: "program-permissions"
                          focusClass: "focusable"
                          slotsee: ["program-files"]
                          refs: []

    $.fn.dataTableExt.oStdClasses.sLength = "dataTables_length form-inline"
    $.fn.dataTableExt.oStdClasses.sFilter = "dataTables_filter form-inline"

    t = $("#program-table");
    return if t.hasClass("dataTable")
    utils.mkDataTable(t)

    kvm['maybeId'].subscribe ->
      t.dataTable().fnAddData [[ kvm['id'](), kvm['label']() ]]

    t.on "click.datatable", "tr", ->
      $("#program-files").html(f)
      id = this.children[0].innerText
      kvm = main.modelSetup("program") viewName, {"id": id},
                            permEl: "program-permissions"
                            focusClass: "focusable"
                            slotsee: ["program-files"]
                            refs: []

      k = global.viewsWare['program-view'].knockVM

    $.getJSON "/all/program",
        (objs) ->
            dt = t.dataTable()
            dt.fnClearTable()
            rows = for obj in objs
                [obj.id.split(':')[1]
                ,obj.label   || ''
                ]
            dt.fnAddData(rows)

    $("#program-add-programPermissions-container").html(
      Mustache.render $("#add-ref-button-template").html(),
              fn:    "addNewPermissionToProgram();"
              label: "Добавить ограничение на поле контракта"
    )

  destructor: ->
  template: tpl