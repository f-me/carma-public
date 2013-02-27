define [
    "utils",
    "model/main",
    "text!tpl/screens/contract.html"],
  (utils, main, tpl) ->
    template: tpl
    constructor: (viewName, args) ->
      setupModel = (args) ->
        main.modelSetup("contract")(
          viewName, args || {"id": null},
            permEl: "contract-permissions"
            focusClass: "focusable"
            refs: []
        )

      setupModel args
      setTimeout ->
        $.fn.dataTableExt.oStdClasses.sLength = "dataTables_length form-inline"
        $.fn.dataTableExt.oStdClasses.sFilter = "dataTables_filter form-inline"

        t = $("#contracts-table")
        return if t.hasClass("dataTable")

        t.on("click.datatable", "tr", ->
          id = this.children[0].innerText
          setupModel {"id": id}
        )

        dt = utils.mkDataTable t

        $.getJSON("/all/contract?fields=id,car_vin",
            (objs) ->
                dt.fnClearTable()
                rows = for obj in objs
                    [obj.id.split(':')[1]
                    ,obj.car_vin || ''
                    ]
                dt.fnAddData(rows)
        )
