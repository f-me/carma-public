define [
    "utils",
    "model/main",
    "text!tpl/screens/contract.html"],
  (utils, main, tpl) ->
    template: tpl
    constructor: (viewName, args) ->
      setupModel = (args) ->
        m = main.modelSetup("contract")(
          viewName, args,
            permEl: "contract-permissions"
            focusClass: "focusable"
            refs: []
            bb: { manual_save: true })
        if args.id
          $('#render-contract').attr(
            "href",
            "/renderContract?prog=1&ctr=#{args.id}")
        return m

      kvm = setupModel args
      setTimeout ->
        sk = mkTableSkeleton global.models.contract,
              [ {name: "#", fn: (o) -> o.id}
              , "ctime"
              , "carVin"
              , "carMake"
              , "carModel"
#              , "carColor"
#              , "carPlateNum"
#              , "carMakeYear"
              , "carCheckPeriod"
#              , "carBuyDate"
#              , "warrantyStart"
#              , "cardNumber"
              , "contractValidFromDate"
              , "contractValidUntilDate"
              , "contractValidUntilMilage"
#              , "milageTO"
#              , "cardOwner"
              , "manager"
#              , "carSeller"
#              , "carDealerTO"
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

        $('#date-min').val (new Date).addDays(-1).toString('yyyy-MM-dd')
        $('#date-max').val (new Date).toString('yyyy-MM-dd')

        fillTable = (objs) ->
          dt.fnClearTable()
          dt.fnAddData(objs.map sk.mkRow)

        $("#filter-btn").on 'click', ->
          min = $('#date-min').val()
          max = $('#date-max').val()
          path = "/allContracts/#{args.program}?from=#{min}&to=#{max}"
          $.getJSON path, fillTable

        $.getJSON "/allContracts/#{args.program}", fillTable

        kvm['maybeId'].subscribe ->
          $.getJSON "/getContract/#{kvm['id']()}", (objs) ->
            dt.fnAddData objs.map sk.mkRow


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
          (v) -> d[v[f]] || v[f] || ''
        else if desc.type == 'date'
          (v) -> if v[f]
              new Date(v[f] * 1000).toString "dd.MM.yyyy"
            else ''
        else if desc.type == 'datetime'
          (v) -> if v[f]
              new Date(v[f] * 1000).toString "dd.MM.yyyy HH:mm:ss"
            else ''
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
