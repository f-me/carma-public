define [ "model/main"
         "utils"
         "text!tpl/screens/program.html"
       ],
(main, utils, tpl) ->
  constructor: (viewName, args) ->
    f = $("#program-files").html()
    kvm = main.modelSetup("program") viewName, args,
                          permEl: "program-permissions"
                          focusClass: "focusable"
                          slotsee: ["program-files"]
                          refs: []
                          bb: { manual_save: true }

    $.fn.dataTableExt.oStdClasses.sLength = "dataTables_length form-inline"
    $.fn.dataTableExt.oStdClasses.sFilter = "dataTables_filter form-inline"

    t = $("#program-table");
    return if t.hasClass("dataTable")
    utils.mkDataTable(t)

    t.on "click.datatable", "tr", ->
      $("#program-files").html(f)
      id = this.children[0].innerText
      kvm = main.modelSetup("program") viewName, {"id": id},
                            permEl: "program-permissions"
                            focusClass: "focusable"
                            slotsee: ["program-files"]
                            refs: []
                            bb: { manual_save: true }
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

  destructor: ->
  template: tpl