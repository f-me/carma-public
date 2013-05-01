define [
    "utils",
    "model/main",
    "text!tpl/screens/contract.html",
    "screenman"],
  (utils, main, tpl, screenman) ->

    reformatDate = (date)->
      [_, d, m, y] = date.match(/([0-9]{2})\/([0-9]{2})\/([0-9]{4})/)
      "#{y}-#{m}-#{d}"

    getContractURL = (id) ->
      "/_/contract/#{id}"

    getContractsURL = (program) ->
      min = reformatDate $('#date-min').val()
      max = reformatDate $('#date-max').val()
      "/allContracts/#{program}?from=#{min}&to=#{max}"

    formatTableColumns = (program) ->
      tableCols =
            [ {name: "#", fn: (o) -> o.id}
            , "isActive"
            , "ctime"
            , "carVin"
            , "carMake"
            , "carModel"
            ]
      if program == '1'
        tableCols.push "carPlateNum"

      tableCols = tableCols.concat(
            [ "contractValidFromDate"
            , "contractValidUntilDate"
            , "contractValidUntilMilage"
            , "manager"
            ])

    mkTableSkeleton = (tableModel, fields) ->
      h = {}
      tableModel.fields.map (f) -> h[f.name] = f

      # remove columns that we don't have in model
      fieldNames = _.pluck tableModel.fields, 'name'
      filterFields = _.filter fields, (e) ->
        return true if typeof e is 'object'
        _.contains(fieldNames, e)

      fs = filterFields.map (f) ->
        if typeof f is 'string'
          desc = h[f]
          {name: desc.meta.label
          ,fn:
            if desc.type is 'dictionary'
              d = global.dictValueCache[desc.meta.dictionaryName]
              (v) -> d[v[f]] || v[f] || ''
            else if desc.type is 'date'
              (v) -> if v[f]
                  new Date(v[f] * 1000).toString "dd.MM.yyyy"
                else ''
            else if desc.type is 'datetime'
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

    dataTableOptions = ->
      # sorting function for 'isActive' column
      $.fn.dataTableExt.afnSortData['dom-checkbox'] = (oSettings, iColumn) ->
        aData = []
        $('td:eq('+iColumn+') input', oSettings.oApi._fnGetTrNodes(oSettings)).each(->
          aData.push(if @checked is true then "1" else "0"))
        aData
      
      aoColumnDefs: [
        sSortDataType: 'dom-checkbox'
        aTargets: [1]
      ]

      fnRowCallback: (nRow, aData, iDisplayIndex, iDisplayIndexFull) ->
        $.getJSON(getContractURL(aData[0]), (contract) ->
          $('td:eq(1)', nRow).html("<input type='checkbox' name='isActive' #{if contract.isActive is "1" then 'checked'} disabled='disabled' />")
        )

    modelSetup = (modelName, viewName, args, programModel) ->
      if args.id
        $('#render-contract').attr(
          "href",
          "/renderContract?prog=#{args.program}&ctr=#{args.id}")

      kvm = main.modelSetup(modelName, programModel)(
        viewName, args,
          permEl: "contract-permissions"
          focusClass: "focusable"
          refs: [])

      if _.find(global.user.roles, (r) -> r == 'contract_user')
        kvm['commentDisabled'](false)  if kvm['commentDisabled']
        kvm['isActiveDisabled'](false) if kvm['isActiveDisabled']
      if _.find(global.user.roles, (r) -> r == 'contract_admin')
        kvm['disableDixi'](true)

      kvm["updateUrl"] = ->
        h = window.location.href.split '/'
        if h[-3..-1][0] == "#contract"
          # /#contract/progid/id case
          u = h[-3..-1].join '/'
          global.router.navigate "#{h[-3..-2].join '/'}/#{kvm['id']()}",
            { trigger: false }
        else
          # /#contract/progid case
          global.router.navigate "#{h[-2..-1].join '/'}/#{kvm['id']()}",
            { trigger: false }

      return kvm

    programSetup = (viewName, args, programModel, programURL) ->
      $('#date-min').val (new Date).addDays(-30).toString('dd/MM/yyyy')
      $('#date-max').val (new Date).toString('dd/MM/yyyy')

      $('#new-contract-btn').on 'click', (e) ->
        e.preventDefault()
        location.hash = "#contract/#{args.program}"
        location.reload(true)

      modelName = "contract"
      kvm = modelSetup modelName, viewName, args, programModel

      tableModelURL = "#{programURL}&field=showtable"

      $.getJSON tableModelURL, (tableModel) ->
        
        sk = mkTableSkeleton tableModel, formatTableColumns args.program
        
        $table = $("#contracts-table")
        $table.append sk.headerHtml
        $table.append "<tbody/>"

        objsToRows = (objs) ->
          objs.map sk.mkRow

        tableParams =
          tableName: "contracts"
          objURL: getContractsURL args.program
        table = screenman.addScreen(modelName, ->)
          .addTable(tableParams)
          .setObjsToRowsConverter(objsToRows)
          .setDataTableOptions(do dataTableOptions)
          .on("click.datatable", "tr", ->
            id = @children[0].innerText
            k  = modelSetup modelName, viewName, {"id": id, "program": args.program}, programModel
            k["updateUrl"]()
            k)

        $("#filter-btn").on 'click', ->
          table.setObjs getContractsURL args.program

        if args.id == null && args.program == '2'
          kvm.carMake 'vw' if kvm.carMake
        kvm.dixi.subscribe ->
          $.getJSON getContractURL kvm['id'](), (objs) ->
            table.dataTable.fnAddData objs.map sk.mkRow

        screenman.showScreen modelName

    screenSetup = (viewName, args) ->
      programURL = "/cfg/model/contract?pid=#{args.program}"
      $.getJSON programURL, (programModel) ->
        programSetup viewName, args, programModel, programURL

    template: tpl
    constructor: screenSetup

