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
        sk = mkTableSkeleton global.models.contract,
              [ {name: "#", fn: (o) -> o.id.split(':')[1]}
              , "carVin"
              , "carMake"
              , "carModel"
              , "carColor"
              , "carPlateNum"
              , "carMakeYear"
              , "carCheckPeriod"
              , "carBuyDate"
              , "warrantyStart"
              , "cardNumber"
              , "contractValidFromDate"
              , "contractValidUntilDate"
              , "contractValidUntilMilage"
              , "milageTO"
              , "cardOwner"
              , "manager"
              , "carSeller"
              , "carDealerTO"
              ]

        $.fn.dataTableExt.oStdClasses.sLength = "dataTables_length form-inline"
        $.fn.dataTableExt.oStdClasses.sFilter = "dataTables_filter form-inline"

        t = $("#contracts-table")
        return if t.hasClass("dataTable")

        t.append sk.headerHtml
        t.append "<tbody/>"

        t.on("click.datatable", "tr", ->
          id = this.children[0].innerText
          setupModel {"id": id}
        )

        dt = utils.mkDataTable t

        $.getJSON("/all/contract"
            (objs) ->
                dt.fnClearTable()
                dt.fnAddData(objs.map sk.mkRow)
        )

mkTableSkeleton = (model, fields) ->
  h = {}
  model.fields.map (f) -> h[f.name] = f

  fs = fields.map (f) ->
    if typeof f == 'string'
      desc = h[f]
      {name: desc.meta.label
      ,fn:
        if desc.type == 'dictionary'
          d = global.dictValueCache[desc.meta.dictionaryName]
          (v) -> d[v[f]] || ''
        else if desc.type == 'date'
          (v) -> if v[f] then new Date(v[f] * 1000).toString "dd.MM.yyyy" else ''
        else
          (v) -> v[f] || ''
      }
    else
      f

  th = $('<thead/>')
  tr = $('<tr/>')
  th.append tr
  fs.map (f) -> tr.append $('<th/>', {html: f.name})

  { mkRow: ((obj) -> fs.map (f) -> f.fn obj)
  , headerHtml: th
  }
