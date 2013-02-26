define [ "model/main"
         "utils"
         "text!tpl/screens/program.html"
       ],
(main, utils, tpl) ->
  constructor: (viewName, args) ->

    kvm = main.modelSetup("program") viewName, args,
                          permEl: "program-permissions"
                          focusClass: "focusable"
                          refs: []

    $.fn.dataTableExt.oStdClasses.sLength = "dataTables_length form-inline"
    $.fn.dataTableExt.oStdClasses.sFilter = "dataTables_filter form-inline"

    t = $("#program-table");
    return if t.hasClass("dataTable")
    utils.mkDataTable(t)

    t.on "click.datatable", "tr", ->
      id = this.children[0].innerText
      kvm = main.modelSetup("program") viewName, {"id": id},
                            permEl: "program-permissions"
                            focusClass: "focusable"
                            refs: []
      k = global.viewsWare['program-view'].knockVM

    $.getJSON "/all/program",
        (objs) ->
            dt = t.dataTable()
            dt.fnClearTable()
            rows = for obj in objs
                [obj.id.split(':')[1]
                ,obj.label   || ''
                ,obj.value   || ''
                ]
            dt.fnAddData(rows)

  destructor: ->
  template: tpl